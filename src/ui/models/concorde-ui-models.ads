with Concorde.Json;

package Concorde.UI.Models is

   type Root_Concorde_Model is abstract tagged private;

   function Handle
     (Model   : in out Root_Concorde_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class
   is abstract;

private

   type Root_Concorde_Model is abstract tagged
      record
         null;
      end record;

end Concorde.UI.Models;
