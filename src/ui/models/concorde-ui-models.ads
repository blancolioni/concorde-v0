with Concorde.Db;
with Nazar.Json;

package Concorde.UI.Models is

   type Root_Concorde_Model is abstract tagged private;

   function Name (Model : Root_Concorde_Model) return String
                  is abstract;

   function Default_View_Name
     (Model : Root_Concorde_Model)
      return String
      is abstract;

   procedure Start
     (Model     : in out Root_Concorde_Model;
      User      : Concorde.Db.User_Reference;
      Arguments : String)
   is null;

   function Handle
     (Model   : in out Root_Concorde_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Nazar.Json.Json_Value'Class)
      return Nazar.Json.Json_Value'Class
   is abstract;

   function Error
     (Model   : in out Root_Concorde_Model'class;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Nazar.Json.Json_Value'Class;
      Message : String)
      return Nazar.Json.Json_Value'Class;

private

   type Root_Concorde_Model is abstract tagged
      record
         null;
      end record;

end Concorde.UI.Models;
