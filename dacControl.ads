with Config; use Config;

with HAL; use HAL;

package dacControl is

   isInit : Boolean := False;

   subtype OutputVoltage_t is Float range -20.0 .. 20.0;
   --  todo set range on DacCode_t
   subtype DacCode_t is Word;

   setVoltage : OutputVoltage_t := 0.0 with Atomic;


   function DacCodeFromVoltage (setVoltage : OutputVoltage_t) return DacCode_t;

   procedure dacInit;

   task dacTask is
      pragma Priority (DAC_TASK_PRIORITY);
   end dacTask;


end dacControl;
