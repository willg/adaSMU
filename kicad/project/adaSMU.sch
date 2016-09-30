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
$Descr A 11000 8500
encoding utf-8
Sheet 1 3
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Sheet
S 4900 3100 1850 1150
U 57EDD8EB
F0 "outputDriver" 60
F1 "outputDriver.sch" 60
F2 "Vset" I L 4900 3650 60 
F3 "Vee" I L 4900 4050 60 
F4 "Vcc" I L 4900 3250 60 
F5 "GND" I R 6750 4050 60 
F6 "Vout" O R 6750 3650 60 
$EndSheet
$Sheet
S 2450 3000 1800 1250
U 57EDDA56
F0 "offsetAndScale" 60
F1 "offsetAndScale.sch" 60
F2 "Vee" I R 4250 4050 60 
F3 "Vcc" I R 4250 3250 60 
F4 "out" O R 4250 3650 60 
F5 "Vin" I L 2450 3550 60 
F6 "Gnd" I L 2450 4100 60 
$EndSheet
$Comp
L GND #PWR01
U 1 1 57EDDE67
P 2250 4250
F 0 "#PWR01" H 2250 4000 50  0001 C CNN
F 1 "GND" H 2250 4100 50  0000 C CNN
F 2 "" H 2250 4250 50  0000 C CNN
F 3 "" H 2250 4250 50  0000 C CNN
	1    2250 4250
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR02
U 1 1 57EDDF52
P 6950 4150
F 0 "#PWR02" H 6950 3900 50  0001 C CNN
F 1 "GND" H 6950 4000 50  0000 C CNN
F 2 "" H 6950 4150 50  0000 C CNN
F 3 "" H 6950 4150 50  0000 C CNN
	1    6950 4150
	1    0    0    -1  
$EndComp
Text Notes 3000 3650 0    60   ~ 0
Vout = m*Vin+b\nm = 8\nb = -12
Wire Wire Line
	4250 3250 4900 3250
Wire Wire Line
	4250 4050 4900 4050
Wire Wire Line
	4250 3650 4900 3650
Wire Wire Line
	2250 4250 2250 4100
Wire Wire Line
	2250 4100 2450 4100
Wire Wire Line
	6950 4150 6950 4050
Wire Wire Line
	6950 4050 6750 4050
Wire Notes Line
	5750 3500 5750 4000
Wire Notes Line
	6000 3750 5500 3750
Text Notes 6050 3800 0    39   ~ 0
Voltage
Text Notes 5750 3500 0    39   ~ 0
Current
Text Notes 5750 3650 0    60   ~ 0
Quad1
Text Notes 5450 3650 0    60   ~ 0
Quad2
Text Notes 5450 3900 0    60   ~ 0
Quad3
Text Notes 5750 3900 0    60   ~ 0
Quad4
Text Notes 1000 3600 0    60   ~ 0
Driven by STM32-DISCO DAC
$EndSCHEMATC
