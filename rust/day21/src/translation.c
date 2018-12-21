#include <stdio.h>

int main()
{
    int reg[] = {0,0,0,0,0,0};
    reg[4] = 123;
    do {
        reg[4] &= 456;
    } while (reg[4] != 72);


    reg[4] = 0;

    do {
        reg[3] = reg[4] | 0x10000;
        reg[4] = 10283511;

        while (1) {
            reg[1] = reg[3] & 0xff;
            reg[4] += reg[1];
            reg[4] &= 0xffffff;
            reg[4] *= 65899;
            reg[4] &= 0xffffff;

            if (256 > reg[3]) {
                break;
            }

            for (reg[1] = 0; ; ++reg[1]) {
                reg[5] = reg[1] + 1;
                reg[5] *= 256;
                if (reg[5] > reg[3]) {
                    break;
                }
            }

            reg[3] = reg[1];
        }
        printf("%d\n", reg[4]);
    } while (reg[4] != reg[0]);
}
