EESchema Schematic File Version 2
LIBS:power
LIBS:device
LIBS:transistors
LIBS:conn
LIBS:linear
LIBS:regul
LIBS:74xx
LIBS:cmos4000
LIBS:adc-dac
LIBS:memory
LIBS:xilinx
LIBS:microcontrollers
LIBS:dsp
LIBS:microchip
LIBS:analog_switches
LIBS:motorola
LIBS:texas
LIBS:intel
LIBS:audio
LIBS:interface
LIBS:digital-audio
LIBS:philips
LIBS:display
LIBS:cypress
LIBS:siliconi
LIBS:opto
LIBS:atmel
LIBS:contrib
LIBS:valves
LIBS:adaSMU
LIBS:stm32
LIBS:adaSMU-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 3 3
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L TL071 U4
U 1 1 57EDDA90
P 5650 3500
F 0 "U4" H 5650 3750 50  0000 L CNN
F 1 "TL071" H 5650 3650 50  0000 L CNN
F 2 "" H 5700 3700 50  0000 C CNN
F 3 "" H 5650 3750 50  0000 C CNN
	1    5650 3500
	1    0    0    1   
$EndComp
Text HLabel 5500 3150 0    60   Input ~ 0
Vee
Text HLabel 5450 3850 0    60   Input ~ 0
Vcc
$Comp
L C_Small C6
U 1 1 57EDDBA0
P 5700 3850
F 0 "C6" H 5710 3920 50  0000 L CNN
F 1 "10uF" H 5710 3770 50  0000 L CNN
F 2 "" H 5700 3850 50  0000 C CNN
F 3 "" H 5700 3850 50  0000 C CNN
	1    5700 3850
	0    1    1    0   
$EndComp
$Comp
L GND #PWR08
U 1 1 57EDDBD2
P 5900 3900
F 0 "#PWR08" H 5900 3650 50  0001 C CNN
F 1 "GND" H 5900 3750 50  0000 C CNN
F 2 "" H 5900 3900 50  0000 C CNN
F 3 "" H 5900 3900 50  0000 C CNN
	1    5900 3900
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR09
U 1 1 57EDDBEC
P 5900 3200
F 0 "#PWR09" H 5900 2950 50  0001 C CNN
F 1 "GND" H 5900 3050 50  0000 C CNN
F 2 "" H 5900 3200 50  0000 C CNN
F 3 "" H 5900 3200 50  0000 C CNN
	1    5900 3200
	1    0    0    -1  
$EndComp
$Comp
L C_Small C5
U 1 1 57EDDC6A
P 5700 3150
F 0 "C5" H 5710 3220 50  0000 L CNN
F 1 "10uF" H 5710 3070 50  0000 L CNN
F 2 "" H 5700 3150 50  0000 C CNN
F 3 "" H 5700 3150 50  0000 C CNN
	1    5700 3150
	0    1    1    0   
$EndComp
Text HLabel 6200 3500 2    60   Output ~ 0
out
$Comp
L R R4
U 1 1 57EDDD04
P 5700 2800
F 0 "R4" V 5780 2800 50  0000 C CNN
F 1 "R" V 5700 2800 50  0000 C CNN
F 2 "" V 5630 2800 50  0000 C CNN
F 3 "" H 5700 2800 50  0000 C CNN
	1    5700 2800
	0    1    1    0   
$EndComp
$Comp
L R R3
U 1 1 57EDDD87
P 4800 3000
F 0 "R3" V 4880 3000 50  0000 C CNN
F 1 "R" V 4800 3000 50  0000 C CNN
F 2 "" V 4730 3000 50  0000 C CNN
F 3 "" H 4800 3000 50  0000 C CNN
	1    4800 3000
	0    1    1    0   
$EndComp
$Comp
L R R2
U 1 1 57EDDDD8
P 4450 3200
F 0 "R2" V 4530 3200 50  0000 C CNN
F 1 "R" V 4450 3200 50  0000 C CNN
F 2 "" V 4380 3200 50  0000 C CNN
F 3 "" H 4450 3200 50  0000 C CNN
	1    4450 3200
	-1   0    0    1   
$EndComp
$Comp
L R R1
U 1 1 57EDDE20
P 4450 2750
F 0 "R1" V 4530 2750 50  0000 C CNN
F 1 "R" V 4450 2750 50  0000 C CNN
F 2 "" V 4380 2750 50  0000 C CNN
F 3 "" H 4450 2750 50  0000 C CNN
	1    4450 2750
	-1   0    0    1   
$EndComp
Text HLabel 4150 3600 0    60   Input ~ 0
Vin
$Comp
L GND #PWR010
U 1 1 57EDDF9D
P 4450 3350
F 0 "#PWR010" H 4450 3100 50  0001 C CNN
F 1 "GND" H 4450 3200 50  0000 C CNN
F 2 "" H 4450 3350 50  0000 C CNN
F 3 "" H 4450 3350 50  0000 C CNN
	1    4450 3350
	1    0    0    -1  
$EndComp
Wire Wire Line
	5500 3150 5600 3150
Wire Wire Line
	5550 3150 5550 3200
Wire Wire Line
	5450 3850 5600 3850
Wire Wire Line
	5550 3850 5550 3800
Wire Wire Line
	5900 3200 5900 3150
Wire Wire Line
	5900 3150 5800 3150
Connection ~ 5550 3150
Connection ~ 5550 3850
Wire Wire Line
	5800 3850 5900 3850
Wire Wire Line
	5900 3850 5900 3900
Wire Wire Line
	6200 3500 5950 3500
Wire Wire Line
	4150 3600 5350 3600
Wire Wire Line
	5050 3400 5350 3400
Wire Wire Line
	5050 2800 5050 3400
Wire Wire Line
	5050 3000 4950 3000
Wire Wire Line
	5050 2800 5550 2800
Connection ~ 5050 3000
Wire Wire Line
	5850 2800 6100 2800
Wire Wire Line
	6100 2800 6100 3500
Connection ~ 6100 3500
Wire Wire Line
	4450 3050 4450 2900
Wire Wire Line
	4650 3000 4450 3000
Connection ~ 4450 3000
Text Label 5550 3850 0    60   ~ 0
Vcc
$Comp
L LT1236 U3
U 1 1 57EDE1AB
P 3700 2700
F 0 "U3" H 3500 3050 60  0000 C CNN
F 1 "LT1236" H 3600 2950 60  0000 C CNN
F 2 "" H 3700 2700 60  0000 C CNN
F 3 "" H 3700 2700 60  0000 C CNN
	1    3700 2700
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR011
U 1 1 57EDE20E
P 3700 3000
F 0 "#PWR011" H 3700 2750 50  0001 C CNN
F 1 "GND" H 3700 2850 50  0000 C CNN
F 2 "" H 3700 3000 50  0000 C CNN
F 3 "" H 3700 3000 50  0000 C CNN
	1    3700 3000
	1    0    0    -1  
$EndComp
Text Label 3250 2600 0    60   ~ 0
Vcc
Wire Wire Line
	4150 2600 4450 2600
Text Label 4200 2600 0    60   ~ 0
Vref
Text Notes 4050 2400 0    60   ~ 0
Vref = 10V
Text HLabel 4150 3850 0    60   Input ~ 0
Gnd
$Comp
L GND #PWR012
U 1 1 57EDE3FA
P 4250 3900
F 0 "#PWR012" H 4250 3650 50  0001 C CNN
F 1 "GND" H 4250 3750 50  0000 C CNN
F 2 "" H 4250 3900 50  0000 C CNN
F 3 "" H 4250 3900 50  0000 C CNN
	1    4250 3900
	1    0    0    -1  
$EndComp
Wire Wire Line
	4250 3850 4250 3900
Wire Wire Line
	4150 3850 4250 3850
$EndSCHEMATC
