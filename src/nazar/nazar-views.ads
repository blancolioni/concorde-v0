with Nazar.Models;
with Nazar.Signals;

package Nazar.Views is

   type Nazar_View_Interface is interface
     and Nazar.Signals.Signal_Source_Interface
     and Nazar.Signals.Signal_Dispatch_Interface;

   procedure Show (View : in out Nazar_View_Interface) is abstract;

   procedure Model_Changed (View : in out Nazar_View_Interface) is abstract;

   function Model
     (View : Nazar_View_Interface)
      return Nazar.Models.Model_Type
   is abstract;

   type Root_View_Type is
     abstract new Nazar_View_Interface
     and Nazar.Signals.Signal_Source_Interface
     and Nazar.Signals.Signal_Dispatch_Interface
   with private;

   subtype View_Class is Root_View_Type'Class;

   type View_Type is access all Root_View_Type'Class;

   overriding function Model
     (View : Root_View_Type)
      return Nazar.Models.Model_Type;

   overriding function Add_Handler
     (View        : in out Root_View_Type;
      Signal      : Nazar.Signals.Signal_Type;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class;
      Handler     : Nazar.Signals.Signal_Handler_Interface'Class)
      return Nazar.Signals.Handler_Id;

   overriding procedure Emit
     (View        : Root_View_Type;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal      : Nazar.Signals.Signal_Type;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class);

   procedure Set_Model
     (View  : not null access Root_View_Type;
      Model : not null access Nazar.Models.Root_Model_Type'Class);

   type Configure_Callback is access
     function (View : not null access Root_View_Type'Class;
               Width, Height : Measure)
               return Boolean;

   procedure On_Configure
     (View : not null access Root_View_Type;
      Handler : Configure_Callback);

private

   type Root_View_Type is
     abstract new Nazar_View_Interface
     and Nazar.Signals.Signal_Source_Interface
     and Nazar.Signals.Signal_Dispatch_Interface with
      record
         Dispatch   : Nazar.Signals.Signal_Handler_Container;
         Base_Model : Nazar.Models.Model_Type;
      end record;

   overriding function Model
     (View : Root_View_Type)
      return Nazar.Models.Model_Type
   is (View.Base_Model);

end Nazar.Views;
