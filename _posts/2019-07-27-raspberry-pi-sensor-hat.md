---
layout: post
title: Raspberry Pi Sense HAT before and after detaching it from the Pi
---

I have a fascination about tracking the temperature inside of our apartment to see how it differs from outside. I already have a Pi using a DHT22 sensor in the bedroom, and now I want to track the temperature in the living room with a [Sense HAT](https://www.raspberrypi.org/products/sense-hat/). After I attached the Sense HAT ontop of the Pi and did some test readings I was less than impressed: temperature readings were over 60 C and humidity readings were less than 20%. After some calibration to the temperature the results were still unsatisfactory, so after a few months I've decided to detach the HAT from the Pi and see that affects the sensor readings.

<!--end excerpt-->

The temperature was calibrated according to the formula

$$
T = \text{HAT temp} - \left(\frac{\text{CPU temp} - \text{HAT temp}}{0.565}\right)
$$

Where the calibration was done against the thermometer on the heat pump in the same room. At around 20-23 C the results were OK, but the higher the real temperature got the Sense HAT reported values a few degrees too hot.

To remove the Sense HAT from the Pi I attached a GPIO ribbon to a breadboard then connected the breadboard to the HAT. There's no reason for using the breadboard other than I don't have any female-male connecters. I attached all of the connectors as described on [pinout](https://pinout.xyz/pinout/sense_hat) and added `dtoverlay=rpi-sense` to `/boot/config.txt`. 

Here are the results:

<figure class="figure">
    <img src="{{ site.url }}/assets/images/sense-hat-before.png" class="img-fluid"/>
    <figcaption class="figure-caption text-center">Before</figcaption>
</figure>
<figure class="figure">
    <img src="{{ site.url }}/assets/images/sense-hat-after.png" class="img-fluid"/>
    <figcaption class="figure-caption text-center">After</figcaption>
</figure>

Immediatly two major differences jump out:

- The humidity is now far more reasonable. We have noticed that our apartment feels a lot drier than our previous place and this show it!
- The temperature is now much more consistant over the aggregated period (5 minutes or whatever Grafana decides).

One result I was hoping for is that the bedroom reading (from another Pi with a different sensor) and the living room would be consistant, but this temperature difference is much higher than I expect. When you walk between the bedroom and living you don't feel any temperature difference so this may be due to the LEDs producing extra heat on the board, but I need to experiment to find out. As a sanity check for the DHT22 sensor I also have a DS18D20 connected to the same Pi which gives consistant readings.

Other than the bigger mess I'm quite happy with the difference removing the HAT did and would recommend everyone to do the same if you're planning on using the HAT in the same way.