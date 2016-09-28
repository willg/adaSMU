with Config; use Config;

package dacControl is

   isInit : Boolean := False;

   procedure dacInit;

   task dacTask is
      pragma Priority (DAC_TASK_PRIORITY);
   end dacTask;


end dacControl;
