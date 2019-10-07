with Concorde.Json;

package Concorde.UI.Models is

   type Root_Concorde_Model is abstract tagged private;

   function Name (Model : Root_Concorde_Model) return String
                  is abstract;

   function Handle
     (Model   : in out Root_Concorde_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class
   is abstract;

   function Error
     (Model   : in out Root_Concorde_Model'class;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Concorde.Json.Json_Value'Class;
      Message : String)
      return Concorde.Json.Json_Value'Class;

private

   type Root_Concorde_Model is abstract tagged
      record
         null;
      end record;

end Concorde.UI.Models;
