

with STM32.Device;  use STM32.Device;
with STM32.Board;   use STM32.Board;

with STM32.GPIO;    use STM32.GPIO;
with Ada.Real_Time; use Ada.Real_Time;

with STM32.DAC;     use STM32.DAC;

package body dacControl is
   Resolution : constant DAC_Resolution := DAC_Resolution_12_Bits;
   --  Arbitrary, change as desired.  Counts will automatically adjust.

   Max_Counts : constant Word := (if Resolution = DAC_Resolution_12_Bits
                                     then Max_12bit_Resolution
                                     else Max_8bit_Resolution);

   procedure dacInit is
   begin
      if isInit then
         return;
      end if;
      isInit := True;
   end dacInit;

   function DacCodeFromVoltage (setVoltage : OutputVoltage_t) return DacCode_t is
      DacVoltage : Float := 0.0;
      DacPercent : Float := 0.0;

   begin
      DacVoltage := (setVoltage + 12.0) / 8.0;
      DacPercent := DacVoltage / 3.0;
      return Word (Float'Rounding (DacPercent * Float (Max_Counts)));
   end DacCodeFromVoltage;


   task body dacTask is
      type upOrDown is (Up, Down);
      use type Word;

      Period : constant Time_Span := Milliseconds (100);  -- arbitrary
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
      K       : Word := 0;
      upDown : upOrDown := Up;

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
         Toggle (TestPoint);


         if upDown = Up then
            K := K + 1;
            if K >= 88 then
               upDown := Down;
            end if;
         else
            K := K - 1;
            if K = 12 then
               upDown := Up;
            end if;
         end if;

         Value := DacCodeFromVoltage (2.0);

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


