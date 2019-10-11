private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Ordered_Maps;
private with WL.String_Maps;

private with Concorde.Json;

with Concorde.Contexts;
with Concorde.Signals;
with Concorde.UI;
with Concorde.UI.Models;

private with Concorde.Db;

package Concorde.Sessions is

   Signal_Clock_Tick : constant Concorde.Signals.Signal_Type :=
     "signal-clock-tick";

   procedure Broadcast
     (Signal : Concorde.Signals.Signal_Type)
   is null;

   function New_Session
     (User_Name : String;
      Password  : String)
     return Concorde.UI.State_Interface'Class;

   function New_Administrator_Session
      return Concorde.UI.State_Interface'Class;

   type Root_Concorde_Session is
     new Concorde.UI.State_Interface with private;

   subtype Concorde_Session is Root_Concorde_Session'Class;

   function Default_Context
     (Session : Root_Concorde_Session'Class)
      return Concorde.Contexts.Context_Type;

private

   package Model_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.UI.Models.Root_Concorde_Model'Class,
        Concorde.UI.Models."=");

   type Client_Type is
      record
         Model   : Model_Holders.Holder;
         Context : Concorde.Contexts.Context_Type;
      end record;

   package Client_Maps is
     new Ada.Containers.Ordered_Maps
       (Concorde.UI.Client_Id, Client_Type, Concorde.UI."<");

   package Environment_Maps is
     new WL.String_Maps (Concorde.Json.Json_Value'Class, Concorde.Json."=");

   type Root_Concorde_Session is
     new Concorde.UI.State_Interface with
      record
         User            : Concorde.Db.User_Reference :=
           Concorde.Db.Null_User_Reference;
         Last_Client     : Concorde.UI.Client_Id := 0;
         Client_Map      : Client_Maps.Map;
         Default_Context : Concorde.Contexts.Context_Type;
         Environment     : Environment_Maps.Map;
      end record;

   overriding function Valid
     (Session   : Root_Concorde_Session)
      return Boolean;

   overriding function Is_Administrator
     (Session   : Root_Concorde_Session)
      return Boolean;

   overriding function User_Name
     (Session   : Root_Concorde_Session)
      return String;

   overriding function New_Client
     (Session        : in out Root_Concorde_Session;
      Model_Name     : String;
      Model_Argument : String)
      return Concorde.UI.Client_Id;

   overriding procedure Replace_Model
     (Session        : in out Root_Concorde_Session;
      Client         : Concorde.UI.Client_Id;
      Model_Name     : String;
      Model_Argument : String);

   overriding procedure Close_Client
     (Session   : in out Root_Concorde_Session;
      Client    : Concorde.UI.Client_Id);

   overriding function Handle_Message
     (Session : in out Root_Concorde_Session;
      Message : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class;

   overriding function Execute_Command
     (Session : in out Root_Concorde_Session;
      Client  : Concorde.UI.Client_Id;
      Command : String)
      return Concorde.Json.Json_Value'Class;

   overriding function Handle_Client_Request
     (Session : in out Root_Concorde_Session;
      Client  : Concorde.UI.Client_Id;
      Request : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class;

   overriding function Environment_Value
     (Session : Root_Concorde_Session;
      Name  : String)
      return Concorde.Json.Json_Value'Class
   is (if Session.Environment.Contains (Name)
       then Session.Environment.Element (Name)
       else Concorde.Json.Null_Value);

   function Default_Context
     (Session : Root_Concorde_Session'Class)
      return Concorde.Contexts.Context_Type
   is (Session.Default_Context);

end Concorde.Sessions;
