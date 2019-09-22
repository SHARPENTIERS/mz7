/* Source modified to compress a MZF file into a MZ7 file */
/*
 * (c) Copyright 2012-2016 by Einar Saukas. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * The name of its author may not be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

/*- zx7.h ---------------------------------------------------*/

#define MAX_OFFSET  2176  /* range 1..2176 */
#define MAX_LEN    65536  /* range 2..65536 */

typedef struct optimal_t {
    size_t bits;
    int offset;
    int len;
} Optimal;

Optimal *optimize(unsigned char *input_data, size_t input_size, long skip);

unsigned char *compress(Optimal *optimal, unsigned char *input_data, size_t input_size, long skip, size_t *output_size, long *delta);

/*- optimze.c -----------------------------------------------*/

int elias_gamma_bits(int value) {
    int bits;

    bits = 1;
    while (value > 1) {
        bits += 2;
        value >>= 1;
    }
    return bits;
}

int count_bits(int offset, int len) {
    return 1 + (offset > 128 ? 12 : 8) + elias_gamma_bits(len-1);
}

Optimal* optimize(unsigned char *input_data, size_t input_size, long skip) {
    size_t *min;
    size_t *max;
    size_t *matches;
    size_t *match_slots;
    Optimal *optimal;
    size_t *match;
    int match_index;
    int offset;
    size_t len;
    size_t best_len;
    size_t bits;
    size_t i;

    /* allocate all data structures at once */
    min = (size_t *)calloc(MAX_OFFSET+1, sizeof(size_t));
    max = (size_t *)calloc(MAX_OFFSET+1, sizeof(size_t));
    matches = (size_t *)calloc(256*256, sizeof(size_t));
    match_slots = (size_t *)calloc(input_size, sizeof(size_t));
    optimal = (Optimal *)calloc(input_size, sizeof(Optimal));

    if (!min || !max || !matches || !match_slots || !optimal) {
         fprintf(stderr, "Error: Insufficient memory\n");
         exit(1);
    }

    /* index skipped bytes */
    for (i = 1; i <= skip; i++) {
        match_index = input_data[i-1] << 8 | input_data[i];
        match_slots[i] = matches[match_index];
        matches[match_index] = i;
    }

    /* first byte is always literal */
    optimal[skip].bits = 8;

    /* process remaining bytes */
    for (; i < input_size; i++) {

        optimal[i].bits = optimal[i-1].bits + 9;
        match_index = input_data[i-1] << 8 | input_data[i];
        best_len = 1;
        for (match = &matches[match_index]; *match != 0 && best_len < MAX_LEN; match = &match_slots[*match]) {
            offset = i - *match;
            if (offset > MAX_OFFSET) {
                *match = 0;
                break;
            }

            for (len = 2; len <= MAX_LEN && i >= skip+len; len++) {
                if (len > best_len) {
                    best_len = len;
                    bits = optimal[i-len].bits + count_bits(offset, len);
                    if (optimal[i].bits > bits) {
                        optimal[i].bits = bits;
                        optimal[i].offset = offset;
                        optimal[i].len = len;
                    }
                } else if (max[offset] != 0 && i+1 == max[offset]+len) {
                    len = i-min[offset];
                    if (len > best_len) {
                        len = best_len;
                    }
                }
                if (i < offset+len || input_data[i-len] != input_data[i-len-offset]) {
                    break;
                }
            }
            min[offset] = i+1-len;
            max[offset] = i;
        }
        match_slots[i] = matches[match_index];
        matches[match_index] = i;
    }

    /* save time by releasing the largest block only, the O.S. will clean everything else later */
    free(match_slots);

    return optimal;
}

/*- compress.c ----------------------------------------------*/

unsigned char* output_data;
size_t output_index;
size_t bit_index;
int bit_mask;
long diff;

void read_bytes(int n, long *delta) {
   diff += n;
   if (diff > *delta)
       *delta = diff;
}

void write_byte(int value) {
    output_data[output_index++] = value;
    diff--;
}

void write_bit(int value) {
    if (bit_mask == 0) {
        bit_mask = 128;
        bit_index = output_index;
        write_byte(0);
    }
    if (value > 0) {
        output_data[bit_index] |= bit_mask;
    }
    bit_mask >>= 1;
}

void write_elias_gamma(int value) {
    int i;

    for (i = 2; i <= value; i <<= 1) {
        write_bit(0);
    }
    while ((i >>= 1) > 0) {
        write_bit(value & i);
    }
}

unsigned char *compress(Optimal *optimal, unsigned char *input_data, size_t input_size, long skip, size_t *output_size, long *delta) {
    size_t input_index;
    size_t input_prev;
    int offset1;
    int mask;
    int i;

    /* calculate and allocate output buffer */
    input_index = input_size-1;
    *output_size = (optimal[input_index].bits+18+7)/8;
    output_data = (unsigned char *)malloc(*output_size);
    if (!output_data) {
         fprintf(stderr, "Error: Insufficient memory\n");
         exit(1);
    }

    /* initialize delta */
    diff = *output_size - input_size + skip;
    *delta = 0;

    /* un-reverse optimal sequence */
    optimal[input_index].bits = 0;
    while (input_index != skip) {
        input_prev = input_index - (optimal[input_index].len > 0 ? optimal[input_index].len : 1);
        optimal[input_prev].bits = input_index;
        input_index = input_prev;
    }

    output_index = 0;
    bit_mask = 0;

    /* first byte is always literal */
    write_byte(input_data[input_index]);
    read_bytes(1, delta);

    /* process remaining bytes */
    while ((input_index = optimal[input_index].bits) > 0) {
        if (optimal[input_index].len == 0) {

            /* literal indicator */
            write_bit(0);

            /* literal value */
            write_byte(input_data[input_index]);
            read_bytes(1, delta);

        } else {

            /* sequence indicator */
            write_bit(1);

            /* sequence length */
            write_elias_gamma(optimal[input_index].len-1);

            /* sequence offset */
            offset1 = optimal[input_index].offset-1;
            if (offset1 < 128) {
                write_byte(offset1);
            } else {
                offset1 -= 128;
                write_byte((offset1 & 127) | 128);
                for (mask = 1024; mask > 127; mask >>= 1) {
                    write_bit(offset1 & mask);
                }
            }
            read_bytes(optimal[input_index].len, delta);
        }
    }

    /* sequence indicator */
    write_bit(1);

    /* end marker > MAX_LEN */
    for (i = 0; i < 16; i++) {
        write_bit(0);
    }
    write_bit(1);

    return output_data;
}

/*- MZF specifics -------------------------------------------*/

typedef struct header_t {
    unsigned char file_attribute;
    char file_name[17];
    unsigned short file_size; 
    unsigned short file_load; 
    unsigned short file_exec;
    char comment[104];
} Header;

void fill_header_and_loader(Header *header, char *loader, size_t skip, size_t output_size, int backwards_mode, long delta) {
    unsigned short original_size = header->file_size;
    unsigned short original_load = header->file_load;
    unsigned short original_exec = header->file_exec;
    char *memory = header->comment - 0x1108;
    size_t loader_size = 0x15;  
    
    printf(
        "[Old file] size: %5d (%04x), load: %04x, exec: %04x\n",
        original_size, original_size, 
        original_load, 
        original_exec);
        
    if (backwards_mode) {
        memcpy(
            header->comment,
            /*
0000                    ORG     0x1108
1108                    ; -----------------------------------------------------------------------------
1108                    ; ZX7 decoder by Einar Saukas & Urusergi
1108                    ; "Turbo" version (88 bytes, 25% faster) - BACKWARDS VARIANT
1108                    ; -----------------------------------------------------------------------------
1108                    ; Parameters:
1108                    ;   HL: last source address (compressed data)
1108                    ;   DE: last destination address (decompressing)
1108                    ; -----------------------------------------------------------------------------
1108                    _main:
1108 */"\x21\x00\x00"/*         ld      hl, $0000                  ; last source address
110B */"\x11\x00\x00"/*         ld      de, $0000                  ; last target address
110E */"\x01\xAD\x00"/*         ld      bc, $00AD                  ; executable address
1111 */"\xC5"        /*         push    bc
1112                    dzx7_turbo_back:
1112 */"\x3E\x80"    /*         ld      a, $80
1114                    dzx7t_copy_byte_loop_b:
1114 */"\xED\xA8"    /*         ldd                                ; copy literal byte
1116                    dzx7t_main_loop_b:
1116 */"\x87"        /*         add     a, a                       ; check next bit
1117 */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits_b       ; no more bits left?
111A */"\x30\xF8"    /*         jr      nc, dzx7t_copy_byte_loop_b ; next bit indicates either literal or sequence
111C
111C                    ; determine number of bits used for length (Elias gamma coding)
111C */"\xD5"        /*         push    de
111D */"\x01\x01\x00"/*         ld      bc, 1
1120 */"\x50"        /*         ld      d, b
1121                    dzx7t_len_size_loop_b:
1121 */"\x14"        /*         inc     d
1122 */"\x87"        /*         add     a, a                       ; check next bit
1123 */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits_b       ; no more bits left?
1126 */"\x30\xF9"    /*         jr      nc, dzx7t_len_size_loop_b
1128 */"\xC3\x35\x11"/*         jp      dzx7t_len_value_start_b
112B
112B                    ; determine length
112B                    dzx7t_len_value_loop_b:
112B */"\x87"        /*         add     a, a                       ; check next bit
112C */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits_b       ; no more bits left?
112F */"\xCB\x11"    /*         rl      c
1131 */"\xCB\x10"    /*         rl      b
1133 */"\x38\x2D"    /*         jr      c, dzx7t_exit_b            ; check end marker
1135                    dzx7t_len_value_start_b:
1135 */"\x15"        /*         dec     d
1136 */"\x20\xF3"    /*         jr      nz, dzx7t_len_value_loop_b
1138 */"\x03"        /*         inc     bc                         ; adjust length
1139
1139                    ; determine offset
1139 */"\x5E"        /*         ld      e, (hl)                    ; load offset flag (1 bit) + offset value (7 bits)
113A */"\x2B"        /*         dec     hl
113B */"\xCB\x33"    /*         defb    $cb, $33                   ; opcode for undocumented instruction "SLL E" aka "SLS E"
113D */"\x30\x1A"    /*         jr      nc, dzx7t_offset_end_b     ; if offset flag is set, load 4 extra bits
113F */"\x87"        /*         add     a, a                       ; check next bit
1140 */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits_b       ; no more bits left?
1143 */"\xCB\x12"    /*         rl      d                          ; insert first bit into D
1145 */"\x87"        /*         add     a, a                       ; check next bit
1146 */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits_b       ; no more bits left?
1149 */"\xCB\x12"    /*         rl      d                          ; insert second bit into D
114B */"\x87"        /*         add     a, a                       ; check next bit
114C */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits_b       ; no more bits left?
114F */"\xCB\x12"    /*         rl      d                          ; insert third bit into D
1151 */"\x87"        /*         add     a, a                       ; check next bit
1152 */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits_b       ; no more bits left?
1155 */"\x3F"        /*         ccf
1156 */"\x38\x01"    /*         jr      c, dzx7t_offset_end_b
1158 */"\x14"        /*         inc     d                          ; equivalent to adding 128 to DE
1159                    dzx7t_offset_end_b:
1159 */"\xCB\x1B"    /*         rr      e                          ; insert inverted fourth bit into E
115B
115B                    ; copy previous sequence
115B */"\xE3"        /*         ex      (sp), hl                   ; store source, restore destination
115C */"\xE5"        /*         push    hl                         ; store destination
115D */"\xED\x5A"    /*         adc     hl, de                     ; HL = destination + offset + 1
115F */"\xD1"        /*         pop     de                         ; DE = destination
1160 */"\xED\xB8"    /*         lddr
1162                    dzx7t_exit_b:
1162 */"\xE1"        /*         pop     hl                         ; restore source address (compressed data)
1163 */"\xD2\x16\x11"/*         jp      nc, dzx7t_main_loop_b
1166
1166                    dzx7t_load_bits_b:
1166 */"\x7E"        /*         ld      a, (hl)                    ; load another group of 8 bits
1167 */"\x2B"        /*         dec     hl
1168 */"\x17"        /*         rla
1169 */"\xC9"        /*         ret
116A
116A                    ; -----------------------------------------------------------------------------
            */,
            0x116A-0x1108
        );

        /*
               header   compressed data   loader
             |--------|-----------------|--------|
                   header           decompressed data            suffix
                 |--------|---------------------------------|--------------|
                                                       << start
                      <--->                                 <-------------->                                 
                      delta                                       skip
        */

        original_size -= skip;
        
        header->file_size = output_size + loader_size;
        header->file_load = original_load - delta;      
        header->file_exec = header->file_load + output_size;
        
        *((unsigned short*)(memory+0x1109)) = original_load - delta + output_size-1;
        *((unsigned short*)(memory+0x110C)) = original_load + original_size-1;
        *((unsigned short*)(memory+0x110F)) = original_exec;    
        
        loader[0x00] = 0x21;                    // 01 xx xx         ld      hl,$xxxx
        loader[0x01] = (original_size >> 0) & 255;
        loader[0x02] = (original_size >> 8) & 255;
        loader[0x03] = 0x22;                    // 22 xx xx         ld      ($1102),hl
        loader[0x04] = 0x02;
        loader[0x05] = 0x11;

        loader[0x06] = 0x21;                    // 01 xx xx         ld      hl,$xxxx
        loader[0x07] = (original_load >> 0) & 255;
        loader[0x08] = (original_load >> 8) & 255;
        loader[0x09] = 0x22;                    // 22 xx xx         ld      ($1104),hl
        loader[0x0A] = 0x04;
        loader[0x0B] = 0x11;

        loader[0x0C] = 0x21;                    // 01 xx xx         ld      h,$xxxx
        loader[0x0D] = (original_exec >> 0) & 255;
        loader[0x0E] = (original_exec >> 8) & 255;
        loader[0x0F] = 0x22;                    // 22 xx xx         ld      ($1106),hl
        loader[0x10] = 0x06;
        loader[0x11] = 0x11;

        loader[0x12] = 0xC3;                    // C3 08 11         jp      0x1108
        loader[0x13] = 0x08;
        loader[0x14] = 0x11;
    } else {
        memcpy(
            header->comment,
            /*
0000                    ORG     0x1108
1108                    ; -----------------------------------------------------------------------------
1108                    ; ZX7 decoder by Einar Saukas & Urusergi
1108                    ; "Turbo" version (88 bytes, 25% faster) - BACKWARDS VARIANT
1108                    ; -----------------------------------------------------------------------------
1108                    ; Parameters:
1108                    ;   HL: last source address (compressed data)
1108                    ;   DE: last destination address (decompressing)
1108                    ; -----------------------------------------------------------------------------
1108                    _main:
1108 */"\x21\x00\x00"/*         ld      hl, $0000                  ; last source address
110B */"\x11\x00\x00"/*         ld      de, $0000                  ; last target address
110E */"\x01\xAD\x00"/*         ld      bc, $00AD                  ; executable address
1111 */"\xC5"        /*         push    bc
1112                    dzx7_turbo_back:
1112 */"\x3E\x80"    /*         ld      a, $80
1114                    dzx7t_copy_byte_loop:
1114 */"\xED\xA0"    /*         ldi                                ; copy literal byte
1116                    dzx7t_main_loop:
1116 */"\x87"        /*         add     a, a                       ; check next bit
1117 */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits         ; no more bits left?
111A */"\x30\xF8"    /*         jr      nc, dzx7t_copy_byte_loop   ; next bit indicates either literal or sequence
111C
111C                    ; determine number of bits used for length (Elias gamma coding)
111C */"\xD5"        /*         push    de
111D */"\x01\x01\x00"/*         ld      bc, 1
1120 */"\x50"        /*         ld      d, b
1121                    dzx7t_len_size_loop:
1121 */"\x14"        /*         inc     d
1122 */"\x87"        /*         add     a, a                       ; check next bit
1123 */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits         ; no more bits left?
1126 */"\x30\xF9"    /*         jr      nc, dzx7t_len_size_loop
1128 */"\xC3\x35\x11"/*         jp      dzx7t_len_value_start
112B
112B                    ; determine length
112B                    dzx7t_len_value_loop:
112B */"\x87"        /*         add     a, a                       ; check next bit
112C */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits         ; no more bits left?
112F */"\xCB\x11"    /*         rl      c
1131 */"\xCB\x10"    /*         rl      b
1133 */"\x38\x2D"    /*         jr      c, dzx7t_exit              ; check end marker
1135                    dzx7t_len_value_start:
1135 */"\x15"        /*         dec     d
1136 */"\x20\xF3"    /*         jr      nz, dzx7t_len_value_loop
1138 */"\x03"        /*         inc     bc                         ; adjust length
1139
1139                    ; determine offset
1139 */"\x5E"        /*         ld      e, (hl)                    ; load offset flag (1 bit) + offset value (7 bits)
113A */"\x23"        /*         inc     hl
113B */"\xCB\x33"    /*         defb    $cb, $33                   ; opcode for undocumented instruction "SLL E" aka "SLS E"
113D */"\x30\x1A"    /*         jr      nc, dzx7t_offset_end       ; if offset flag is set, load 4 extra bits
113F */"\x87"        /*         add     a, a                       ; check next bit
1140 */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits         ; no more bits left?
1143 */"\xCB\x12"    /*         rl      d                          ; insert first bit into D
1145 */"\x87"        /*         add     a, a                       ; check next bit
1146 */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits         ; no more bits left?
1149 */"\xCB\x12"    /*         rl      d                          ; insert second bit into D
114B */"\x87"        /*         add     a, a                       ; check next bit
114C */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits         ; no more bits left?
114F */"\xCB\x12"    /*         rl      d                          ; insert third bit into D
1151 */"\x87"        /*         add     a, a                       ; check next bit
1152 */"\xCC\x66\x11"/*         call    z, dzx7t_load_bits         ; no more bits left?
1155 */"\x3F"        /*         ccf
1156 */"\x38\x01"    /*         jr      c, dzx7t_offset_end
1158 */"\x14"        /*         inc     d                          ; equivalent to adding 128 to DE
1159                    dzx7t_offset_end:
1159 */"\xCB\x1B"    /*         rr      e                          ; insert inverted fourth bit into E
115B
115B                    ; copy previous sequence
115B */"\xE3"        /*         ex      (sp), hl                   ; store source, restore destination
115C */"\xE5"        /*         push    hl                         ; store destination
115D */"\xED\x52"    /*         sbc     hl, de                     ; HL = destination - offset + 1
115F */"\xD1"        /*         pop     de                         ; DE = destination
1160 */"\xED\xB0"    /*         ldir
1162                    dzx7t_exit:
1162 */"\xE1"        /*         pop     hl                         ; restore source address (compressed data)
1163 */"\xD2\x16\x11"/*         jp      nc, dzx7t_main_loop
1166
1166                    dzx7t_load_bits:
1166 */"\x7E"        /*         ld      a, (hl)                    ; load another group of 8 bits
1167 */"\x23"        /*         inc     hl
1168 */"\x17"        /*         rla
1169 */"\xC9"        /*         ret
116A
116A                    ; -----------------------------------------------------------------------------
            */,
            0x116A-0x1108
        );
    
        /*
                                           header   loader   compressed data
                                         |--------|--------|-----------------|
                 header      prefix             decompressed data
               |--------|--------------|---------------------------------|
                                        start >>
                        <-------------->                                 <--->
                              skip                                       delta
        */

        original_size -= skip;
        original_load += skip;
        
        if (delta < loader_size) {
            delta = loader_size;
        }
        
        header->file_size = loader_size + output_size;
        header->file_load = original_load + original_size + delta - header->file_size;      
        header->file_exec = header->file_load;
        
        *((unsigned short*)(memory+0x1109)) = original_load + original_size + delta - output_size;
        *((unsigned short*)(memory+0x110C)) = original_load;
        *((unsigned short*)(memory+0x110F)) = original_exec;
        
        loader[0x00] = 0x21;                    // 01 xx xx         ld      hl,$xxxx
        loader[0x01] = (original_size >> 0) & 255;
        loader[0x02] = (original_size >> 8) & 255;
        loader[0x03] = 0x22;                    // 22 xx xx         ld      ($1102),hl
        loader[0x04] = 0x02;
        loader[0x05] = 0x11;

        loader[0x06] = 0x21;                    // 01 xx xx         ld      hl,$xxxx
        loader[0x07] = (original_load >> 0) & 255;
        loader[0x08] = (original_load >> 8) & 255;
        loader[0x09] = 0x22;                    // 22 xx xx         ld      ($1104),hl
        loader[0x0A] = 0x04;
        loader[0x0B] = 0x11;

        loader[0x0C] = 0x21;                    // 01 xx xx         ld      h,$xxxx
        loader[0x0D] = (original_exec >> 0) & 255;
        loader[0x0E] = (original_exec >> 8) & 255;
        loader[0x0F] = 0x22;                    // 22 xx xx         ld      ($1106),hl
        loader[0x10] = 0x06;
        loader[0x11] = 0x11;

        loader[0x12] = 0xC3;                    // C3 08 11         jp      0x1108
        loader[0x13] = 0x08;
        loader[0x14] = 0x11;
    }

    printf(
        "[New file] size: %5d (%04x), load: %04x, exec: %04x\n",
        header->file_size, header->file_size, 
        header->file_load, header->file_exec);  
}

/*- zx7.c (modified to compress MZF into MZ7 ----------------*/

long parse_long(char *str) {
    long value;

    errno = 0;
    value = strtol(str, NULL, 10);
    return !errno ? value : LONG_MIN;
}

void reverse(unsigned char *first, unsigned char *last) {
    unsigned char c;

    while (first < last) {
        c = *first;
        *first++ = *last;
        *last-- = c;
    }
}

int main(int argc, char *argv[]) {
    long skip = 0;
    int forced_mode = 0;
    int backwards_mode = 0;
    char *input_name;
    char *output_name;
    unsigned char *input_data;
    unsigned char *output_data;
    FILE *ifp;
    FILE *ofp;
    size_t input_size;
    size_t output_size;
    size_t partial_counter;
    size_t total_counter;
    long delta;
    int i;
    char loader_data[0x15];

    /* process hidden optional parameters */
    for (i = 1; i < argc && (*argv[i] == '-' || *argv[i] == '+'); i++) {
        if (!strcmp(argv[i], "-f")) {
            forced_mode = 1;
        } else if (!strcmp(argv[i], "-b")) {
            backwards_mode = 1;
        } else if ((skip = parse_long(argv[i])) <= 0) {
            fprintf(stderr, "Error: Invalid parameter %s\n", argv[i]);
            exit(1);
        }
    }

    /* determine output filename */
    if (argc == i+1) {
        input_name = argv[i];
        input_size = strlen(input_name);
        if (input_size > 4 && !strcmp(input_name+input_size-4, ".mzf")) {
            output_name = (char *)malloc(input_size);
            strcpy(output_name, input_name);
            output_name[input_size-1] = '7';
        } else {
            fprintf(stderr, "Error: Cannot infer input filename\n");
            exit(1);
        }
    } else if (argc == i+2) {
        output_name = argv[i+1];
    } else {
        fprintf(stderr, "mz7c: using optimal LZ77/LZSS compression by Einar Saukas\n");
        fprintf(stderr, "Usage: %s [-f] [-b] input.mzf [output.mz7]\n"
                        "  -f      Force overwrite of output file\n"
                        "  -b      Compress backwards\n", argv[0]);


        exit(1);
    }

    /* open input file */
    ifp = fopen(argv[i], "rb");
    if (!ifp) {
        fprintf(stderr, "Error: Cannot access input file %s\n", argv[i]);
        exit(1);
    }

    /* determine input size */
    fseek(ifp, 0L, SEEK_END);
    input_size = ftell(ifp);
    fseek(ifp, 0L, SEEK_SET);
    if (!input_size || input_size <= 128) {
        fprintf(stderr, "Error: Empty input file %s\n", argv[i]);
        exit(1);
    }

    /* validate skip against input size */
    if (skip >= input_size) {
        fprintf(stderr, "Error: Skipping entire input file %s\n", argv[i]);
        exit(1);
    }

    /* allocate input buffer */
    input_data = (unsigned char *)malloc(input_size);
    if (!input_data) {
        fprintf(stderr, "Error: Insufficient memory\n");
        exit(1);
    }

    /* read input file */
    total_counter = 0;
    do {
        partial_counter = fread(input_data+total_counter, sizeof(char), input_size-total_counter, ifp);
        total_counter += partial_counter;
    } while (partial_counter > 0);

    if (total_counter != input_size) {
        fprintf(stderr, "Error: Cannot read input file %s\n", argv[i]);
        exit(1);
    }

    /* close input file */
    fclose(ifp);

    /* check output file */
    if (!forced_mode && fopen(output_name, "rb") != NULL) {
        fprintf(stderr, "Error: Already existing output file %s\n", output_name);
        exit(1);
    }

    /* create output file */
    ofp = fopen(output_name, "wb");
    if (!ofp) {
        fprintf(stderr, "Error: Cannot create output file %s\n", output_name);
        exit(1);
    }

    /* conditionally reverse input file */
    if (backwards_mode) {
        reverse(input_data+sizeof(Header), input_data+input_size-1);
    }

    
    /* generate output file */
    output_data = compress(optimize(input_data+sizeof(Header), input_size-sizeof(Header), skip), input_data+sizeof(Header), input_size-sizeof(Header), skip, &output_size, &delta);

    /* conditionally reverse output file */
    if (backwards_mode) {
        reverse(output_data, output_data+output_size-1);
    }

    fill_header_and_loader((Header *)input_data, loader_data, skip, output_size, backwards_mode, delta);
    
    /* write output file */
    if (fwrite(input_data, sizeof(char), sizeof(Header), ofp) != sizeof(Header)) {
        fprintf(stderr, "Error: Cannot write output file %s\n", output_name);
        exit(1);
    }
    if (!backwards_mode && fwrite(loader_data, sizeof(char), sizeof(loader_data), ofp) != sizeof(loader_data)) {
        fprintf(stderr, "Error: Cannot write output file %s\n", output_name);
        exit(1);
    }
    if (fwrite(output_data, sizeof(char), output_size, ofp) != output_size) {
        fprintf(stderr, "Error: Cannot write output file %s\n", output_name);
        exit(1);
    }
    if (backwards_mode && fwrite(loader_data, sizeof(char), sizeof(loader_data), ofp) != sizeof(loader_data)) {
        fprintf(stderr, "Error: Cannot write output file %s\n", output_name);
        exit(1);
    }
    
    /* close output file */
    fclose(ofp);

    /* done! */
    printf("Data%s converted%s from %lu to %lu bytes! (delta %ld)\n", (skip ? " partially" : ""), (backwards_mode ? " backwards" : ""), 
        (unsigned long)(input_size-skip-128), (unsigned long)output_size, delta);

    return 0;
}
