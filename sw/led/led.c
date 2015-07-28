// See LICENSE for license details.

#include "zscale.h"

void led_set(int output)
{
  *(volatile int*)LED_ADDR = output;
}

int main()
{
  led_set(0xf);
  led_set(0x9);

  return 0;
}
