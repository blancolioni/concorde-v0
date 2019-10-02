private with Ada.Containers.Ordered_Maps;

with Concorde.Signals;
with Concorde.UI;

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

private

   type Client_Type is
      record
         null;
      end record;

   package Client_Maps is
     new Ada.Containers.Ordered_Maps
       (Concorde.UI.Client_Id, Client_Type, Concorde.UI."<");

   type Root_Concorde_Session is
     new Concorde.UI.State_Interface with
      record
         User        : Concorde.Db.User_Reference :=
           Concorde.Db.Null_User_Reference;
         Last_Client : Concorde.UI.Client_Id := 0;
         Client_Map  : Client_Maps.Map;
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
     (Session   : in out Root_Concorde_Session)
      return Concorde.UI.Client_Id;

   overriding procedure Close_Client
     (Session   : in out Root_Concorde_Session;
      Client    : Concorde.UI.Client_Id);

end Concorde.Sessions;
