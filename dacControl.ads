with HAL; use HAL;

package dacControl is

   isInit : Boolean := False;

   subtype OutputVoltage_t is Float range -20.0 .. 20.0;
   --  todo set range on DacCode_t
   subtype DacCode_t is Word;

   procedure dacInit;
   procedure dacSetOutput (setPoint : OutputVoltage_t);
   function DacCodeFromVoltage (setVoltage : OutputVoltage_t) return DacCode_t;


end dacControl;
