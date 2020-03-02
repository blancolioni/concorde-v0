package body Nazar.Views is

   type Model_Observer is
     new Nazar.Models.Model_Observer_Interface with
      record
         View : View_Type;
      end record;

   overriding procedure Notify
     (Observer : Model_Observer);

   -----------------
   -- Add_Handler --
   -----------------

   overriding function Add_Handler
     (View        : in out Root_View_Type;
      Signal      : Nazar.Signals.Signal_Type;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class;
      Handler     : Nazar.Signals.Signal_Handler_Interface'Class)
      return Nazar.Signals.Handler_Id
   is
   begin
      return View.Dispatch.Add_Handler
        (Signal, Source, User_Data, Handler);
   end Add_Handler;

   ----------
   -- Emit --
   ----------

   overriding procedure Emit
     (View        : Root_View_Type;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal      : Nazar.Signals.Signal_Type;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class)
   is
   begin
      View.Dispatch.Emit (Source, Signal, Signal_Data);
   end Emit;

   ------------
   -- Notify --
   ------------

   overriding procedure Notify
     (Observer : Model_Observer)
   is
   begin
      Observer.View.Model_Changed;
   end Notify;

   ------------------
   -- On_Configure --
   ------------------

   procedure On_Configure
     (View    : not null access Root_View_Type;
      Handler : Configure_Callback)
   is null;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (View  : not null access Root_View_Type;
      Model : not null access Nazar.Models.Root_Model_Type'Class)
   is
   begin
      View.Base_Model := Nazar.Models.Model_Type (Model);
      View.Base_Model.Add_Observer
        (Model_Observer'(View => View_Type (View)));
      Root_View_Type'Class (View.all).Model_Changed;
   end Set_Model;

end Nazar.Views;
