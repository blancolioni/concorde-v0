package body Concorde.UI.Models.Shell is

   type Shell_Model_Type is
     new Root_Concorde_Model with
      record
         null;
      end record;

   overriding function Name
     (Model : Shell_Model_Type)
      return String
   is ("shell");

   overriding function Default_View_Name
     (Model : Shell_Model_Type)
      return String
   is ("Shell");

   overriding function Handle
     (Model   : in out Shell_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Model   : in out Shell_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class
   is
      pragma Unreferenced (Model);
      Command : constant String :=
        Concorde.Json.Json_Object (Request).Get_Property ("data");
      Response : constant Concorde.Json.Json_Value'Class :=
          State.Execute_Command (Client, Command);
   begin
      return Response;
   end Handle;

   -----------------
   -- Shell_Model --
   -----------------

   function Shell_Model
     (Arguments : String)
      return Root_Concorde_Model'Class
   is
      pragma Unreferenced (Arguments);
   begin
      return Model : Shell_Model_Type;
   end Shell_Model;

end Concorde.UI.Models.Shell;
