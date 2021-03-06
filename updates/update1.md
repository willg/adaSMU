Background
==========

Our project is to build a source measurement unit (SMU).  An SMU is an
electronics test instrument, like an oscilloscope or multimeter. An SMU is
probably most similar to a power supply, but has additional features which make
it useful for a wider variety of measurements and experiments. 

With a normal power supply a user can set an output voltage and the supply will
provide current to a circuit under test at this voltage.  This is useful for
powering a circuit under test.  This is known as quadrant 1 operation for
a positive supply (positive voltage, positive current) or quadrant 3 operation
(negative voltage, negative current) for a negative supply.  An SMU can operate
in these quadrants as well, but in addition it can operate in quadrants
2 (positive voltage, negative current) and quadrant 4 (negative voltage,
positive current).  In these additional modes of operation, the SMU is actually
not supplying power to the circuit under test.  Instead, it is loading the
circuit under test.  This feature enables many useful applications for the SMU.
For instance you can simulate a load on your circuit that may be too expensive
or dangerous to test with directly. For example, if you are building a battery
charger you could test it by "charging" an SMU instead of a real battery so
that if your charger circuit has an error it will not damage a real battery
(which can be unsafe).

An additional major feature of an SMU is it's ability to accurately measure
it's own output voltage and current and report them to the user.  This feature
enables applications, such as measuring diode curves, measuring resistance,
accurately measuring sleep currents and many more.


### Early June
We began in early June by writing our project proposal.  Next we wrote the high
level features we thought our project would make our project a useful instrument:

* 4 quadrant operation
* +/-12V output voltage range
* +-2A output current range
* output voltage measurement
* output current measurement
* Multiple Operating modes:
 * Constant Voltage, Current compliance mode
 * Constant Current, Voltage compliance mode
 * Single quadrant DC load mode

Next we determined what components would be needed to meet those performance
objectives:

* A User interface
* Hardware to generate output voltage and current
* Hardware to measure output voltage and current
* An output voltage control loop
* An output current control loop

We decided that the STM32F429 DISCOVERY would be a good match to our
requirements.  It features a 2.4" touchscreen LCD, which we could use for our
user interface.  It has two, fast 12 bit DACs which can be used for controlling
an output power stage.  It has a built in 12bit 2.4MSPS ADCs which we can use
to measure output voltage and current.  Finally, it runs at a clock rate of up
to 180MHz, affording it the processing power necessary to run two real-time
control loops.

### July-August
In July we began experimenting with Ada on the STM32F429 Discovery board by trying 
all of the different example projects, and then expanding upon the "hello_world_blinky"
project to read button presses and write to the LCD.  

### September

September is when progress on the project really began again in earnest. We
made significant progress on the hardware and built enough software to begin
demonstrating basic functionality of that hardware.

Hardware
========

Our hardware architecture consists of the following functional blocks:
1. Output stage
2. Offset and scale
3. Output voltage measurement
4. Output current measurement

### Output stage

The function of the output stage is to generate the output voltages and
currents required by our SMU.  To this end it must be able to generate both
positive and negative voltages, and to source and sink significant current.
Our chosen design is an application circuit from Linear Technologies of the
LT3091 and LT3081 linear regulator IC's. The LT3091 and LT3081 both share
a novel current reference based architecture which allows them to be used to
directly buffer another input signal, but with high current drive.  The LT3081
is the positive output version and the LT3091 is the negative output version.
By combining them, you arrive at a circuit which is able to generate either
positive or negative voltage and both source and sink current.

We built this circuit up on a piece of copper-clad FR-4 and tested it on the
bench.  Besides the LT3081 and LT3091 IC's, we built it primarily using
scavanged or scrap parts we had on hand and even made our own low-value ballast
resistors out of lengths of wire.

![alt text](https://dl.dropboxusercontent.com/s/yfhr9g2wc0lo6k3/OutputDriver.png?dl=0s/ous4vje1ny2rs39/OffsetAndScale.png "Output Driver")

### Offset and Scale

With the output stage built and working, we next had to interface it to the
STM32F429 board.  Since the output voltage of the output stage swings +/-12V
and needs a control signal that also swings in that range, the on board DAC,
which only swings 0-3V is not suitable for directly driving the output stage.
The job of the offset and scale circuit is to take the 0-3V scale of the STM32
DAC and map it linearly to the +-12V range of the output stage. What is needed
is a circuit which performs the function Vout = m\*Vin + b. Specifically, b is
-12V since we want the driver to output -12V when the DAC outputs zero, and
m is 8V because we want the output to rise 24V to +12V when the DAC rises to
3V. We used a Texas Instruments app note to design this circuit around a low
cost TL072 op-amp and LT1236 10V reference we had available.  After fixing
a brief pinout issue with TL072, the circuit was functioning as designed and we
were controlling output voltages from the STM32 DAC. Our offset and scale
circuit schematic is shown below.

![alt text](https://dl.dropboxusercontent.com/s/ous4vje1ny2rs39/OffsetAndScale.png "Offset and Scale Circuit")

Pictures of our prototype board are shown below:

![alt text](https://dl.dropboxusercontent.com/s/x3dkkw9qmwv9692/benchWproto.jpg "Prototype circuit")

Software
========

### Architecture

The system is broken into two separate tasks -- the `dacTask` and the `UI` task.
The `UI` task manages the user interface, and the `dacTasks` manages the DAC set-
point given a desired set point.  Each of the tasks are outlined below:

### User Interface

The user interface (`UI`) displays the SMU's set point on the STM32F429
Discovery board's LCD, and accepts input from both the push button and the
touch screen.  The push button zeros the SMU's output, and the touch screen has
four virtual buttons, which allows the user to change the output in 1V and 0.1V
increments.

When the user changes the desired output voltage via the touchscreen or
a button, the UI task updates the LCD, and then updates the `dacTask`'s set
point.  

### DAC Control

The DAC is controlled by a dedicated task that exposes an atomic variable to
allow other tasks to set the desired output voltage.  At this point in the
development, a task is probably more overhead than required, and the system
could be better served with a procedure called to set the output voltage.  We
decided to keep the DAC controlled by a dedicated task because we plan to have
a control loop around the DAC with the desired voltage, desired current,
measured voltage, and measured current as inputs.  Putting this control loop in
a dedicated task with the appropriate priority level would allow the system to
maintain the required real-time response to interface safely with loads such as
battery charging.  

### Offset and Scale Correction

In order to drive the +-12V output, the 0-3V DAC output must be scaled to match
the characteristics of the offset and scale hardware.  Starting with the
approximate model of Vout = 8 * dacVoltage - 12, we measured the actual slope
and intercept, adjusted our model and achieved excellent correlation between
the desired set point and the measured SMU output voltage.  In the following
images show the agreement between the SMU output voltage set on the Discovery
board, and the actual output voltage measured on the Keithley volt meter.  

![alt text](https://dl.dropboxusercontent.com/s/d3q3asdv6h2huk7/tracking1.jpg "Tracking at +1V out")

![alt text](https://dl.dropboxusercontent.com/s/x5nd3jqgtv0awc0/tracking2.jpg "Tracking at -1V out")

![alt text](https://dl.dropboxusercontent.com/s/8euqc11oc43cyru/tracking4.jpg "Tracking at -4.2V out")

![alt text](https://dl.dropboxusercontent.com/s/meez84mgjip9599/tracking5.jpg "Tracking at 6.7V out")

### Existing Software Improvements

* Encapsulate atomic global variable `setVoltage` with protections and a procedure
* Incorporate data hiding to make appropriate variables private
* Add SPARK restrictions on ouput voltage range and `UI`
* Re-factor `UI` task code to make flow clearer

### Future Software Work 

* Get ADC working -- measure output voltage and output current
* Control loop around measured output voltage and current
* Improve UI
* Implement constant current functional mode
* Implemnet battery charger functional mode
* Implement DC load functional mode

Accomplishments and Summary
===========================

Although we did not completely finish our SMU, we did demonstrate some basic
initial SMU functionality.  We also got our feet wet with Ada and learned a lot
along the way. Some of our accomplishments were:

###Open
* Published our schematics and source code on GitHub:
[link](https://github.com/willg/adaSMU)
* Used open source Kicad software to draw our schematics

###Collaborative
* We wrote this post explaining in detail our work, so that others can learn
  from it

###Dependable
* Code is readable and commented

###Inventive
* We built a driver circuit with full four quadrant operation
* We interfaced our circuit with Ada software on an ARM Cortex-M
* We used Ada in the test equipment domain, where to our knowledge it has not
  been used before

