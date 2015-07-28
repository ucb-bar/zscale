// See LICENSE for license details.

#include "zscale.h"

unsigned int lfsr_reg = 1;

void lfsr_init()
{
  lfsr_reg = 1;
}

unsigned int lfsr_next()
{
  return lfsr_reg = (lfsr_reg>>1)^(-(lfsr_reg&1) & 0xd0000001);
}

void led_set(int output)
{
  *(volatile int*)LED_ADDR = output;
}

void led_toggle()
{
  static int leds = 0;

  leds++;
  if (leds == 0x10)
    leds = 0;
  led_set(leds);
}

int main()
{
  volatile int* dram = (int*)DRAM_ADDR;

  lfsr_init();
  for (int i=0; i<DRAM_SIZE/4; i++) {
    dram[i] = lfsr_next();
    if ((i & 0xfffff) == 0) led_toggle();
  }

  lfsr_init();
  for (int i=0; i<DRAM_SIZE/4; i++) {
    if (dram[i] != lfsr_next())
      return 1;
    if ((i & 0xfffff) == 0) led_toggle();
  }

  return 0;
}
