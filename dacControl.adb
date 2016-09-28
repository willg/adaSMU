

with HAL; use HAL;
with STM32.Device;  use STM32.Device;
with STM32.Board;   use STM32.Board;

with STM32.GPIO;    use STM32.GPIO;
with Ada.Real_Time; use Ada.Real_Time;

with STM32.DAC;     use STM32.DAC;

package body dacControl is

   procedure dacInit is
   begin
      if isInit then
         return;
      end if;
      isInit := True;
   end dacInit;

   task body dacTask is
      use type Word;

      Period : constant Time_Span := Milliseconds (1000);  -- arbitrary
--      subtype counts is Integer range 0 .. 10;
--      count : counts := 0;

      Next_Release : Time := Clock;

      Output_Channel : constant DAC_Channel := Channel_1;
      procedure ConfigureDAC_GPIO (Output_Channel : DAC_Channel);

      procedure ConfigureDAC_GPIO (Output_Channel : DAC_Channel) is
         Output : constant GPIO_Point := (if Output_Channel = Channel_1
                                          then DAC_Channel_1_IO
                                          else DAC_Channel_2_IO);
         Config : GPIO_Port_Configuration;
      begin
         Enable_Clock (Output);
         Config.Mode := Mode_Analog;
         Config.Resistors := Floating;
         Configure_IO (Output, Config);
      end ConfigureDAC_GPIO;


      Value   : Word := 0;
      Percent : Word;
      K       : Word := 0;

      Resolution : constant DAC_Resolution := DAC_Resolution_12_Bits;
      --  Arbitrary, change as desired.  Counts will automatically adjust.

      Max_Counts : constant Word := (if Resolution = DAC_Resolution_12_Bits
                                     then Max_12bit_Resolution
                                     else Max_8bit_Resolution);

   begin
      ConfigureDAC_GPIO (Output_Channel);
      Enable_Clock (DAC_1);
      Reset (DAC_1);
      Select_Trigger (DAC_1, Output_Channel, Software_Trigger);
      Enable_Trigger (DAC_1, Output_Channel);
      Enable (DAC_1, Output_Channel);

      Next_Release := Next_Release + Period;
      delay until Next_Release;
      loop
         Toggle (Red);

         Percent := K * 10;
         K := K + 1;

         Value := (Percent * Max_Counts) / 100;

         Set_Output
            (DAC_1,
             Output_Channel,
             Value,
             Resolution,
             Right_Aligned);

         Trigger_Conversion_By_Software (DAC_1, Output_Channel);

         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end dacTask;
end dacControl;


