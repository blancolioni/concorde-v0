with AWS.Net.WebSocket.Registry.Control;
with AWS.Server;
with AWS.Status;

with Concorde.UI.Web_UI.Handlers;
with Concorde.UI.Web_UI.Logging;
with Concorde.UI.Web_UI.Routes;

package body Concorde.UI.Web_UI is

   type Socket_Type is
     new AWS.Net.WebSocket.Object
     and Concorde.UI.Connection_Interface
   with null record;

   overriding procedure On_Close
     (Socket : in out Socket_Type;
      Message : String);

   overriding procedure On_Error
     (Socket  : in out Socket_Type;
      Message : String);

   overriding procedure On_Message
     (Socket  : in out Socket_Type;
      Message : String);

   overriding procedure On_Open
     (Socket  : in out Socket_Type;
      Message : String);

   overriding procedure Send_Message
     (Socket     : in out Socket_Type;
      Message    : Concorde.Json.Json_Value'Class);

   function Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data)
      return AWS.Net.WebSocket.Object'Class;

   type Web_UI_Type is
     new UI_Interface with
      record
         null;
      end record;

   overriding procedure Start
     (Web_UI  : in out Web_UI_Type);

   overriding procedure Stop
     (Item    : in out Web_UI_Type;
      Message : String);

   Local_Web_UI : aliased Web_UI_Type;
   Server : AWS.Server.HTTP;

   procedure Create_Routes;

   procedure Create_Socket;

   ------------
   -- Create --
   ------------

   function Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data)
      return AWS.Net.WebSocket.Object'Class
   is
   begin
      return Socket_Type'
        (AWS.Net.WebSocket.Object
           (AWS.Net.WebSocket.Create (Socket, Request)) with null record);
   end Create;

   -------------------
   -- Create_Routes --
   -------------------

   procedure Create_Routes is
   begin
      Routes.Add_Route
        (Method  => AWS.Status.POST,
         Path    => "/login",
         Handler => Handlers.Handle_Login);
      Routes.Add_Route
        (Method  => AWS.Status.POST,
         Path    => "/new-client",
         Handler => Handlers.Handle_New_Client);
      Routes.Add_Route
        (Method  => AWS.Status.GET,
         Path    => "/environment/:name",
         Handler => Handlers.Handle_Environment_Request);
      Routes.Add_Route
        (Method  => AWS.Status.POST,
         Path    => "/client/:client",
         Handler => Handlers.Handle_Client_Request);
   end Create_Routes;

   -------------------
   -- Create_Socket --
   -------------------

   procedure Create_Socket is
   begin
      AWS.Net.WebSocket.Registry.Register
        (URI     => "/socket",
         Factory => Create'Access);
      AWS.Net.WebSocket.Registry.Control.Start;
   end Create_Socket;

   ----------------
   -- Get_Web_UI --
   ----------------

   function Get_Web_UI return UI_Type is
   begin
      return Local_Web_UI'Access;
   end Get_Web_UI;

   --------------
   -- On_Close --
   --------------

   overriding procedure On_Close
     (Socket  : in out Socket_Type;
      Message : String)
   is
      pragma Unreferenced (Socket);
   begin
      null;
   end On_Close;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (Socket  : in out Socket_Type;
      Message : String)
   is
      pragma Unreferenced (Socket);
   begin
      null;
   end On_Error;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Socket  : in out Socket_Type;
      Message : String)
   is
      Response : constant String :=
        Routes.Handle_Socket_Message
          (Message);
   begin
      Socket.Send (Message => Response);
   end On_Message;

   -------------
   -- On_Open --
   -------------

   overriding procedure On_Open
     (Socket  : in out Socket_Type;
      Message : String)
   is
      pragma Unreferenced (Socket);
   begin
      null;
   end On_Open;

   ------------------
   -- Send_Message --
   ------------------

   overriding procedure Send_Message
     (Socket     : in out Socket_Type;
      Message    : Concorde.Json.Json_Value'Class)
   is
   begin
      Socket.Send
        (Message => Message.Serialize);
   end Send_Message;

   -----------
   -- Start --
   -----------

   overriding procedure Start (Web_UI  : in out Web_UI_Type) is
      pragma Unreferenced (Web_UI);
   begin

      Logging.On_Starting;

      Create_Routes;
      Create_Socket;

      AWS.Server.Start
        (Web_Server => Server,
         Name       => "Concorde",
         Callback   => Routes.Handle_Http_Request'Access,
         Port       => 8080);

   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (Item    : in out Web_UI_Type;
      Message : String)
   is
      pragma Unreferenced (Item);
   begin
      Logging.On_Stopping (Message);
      AWS.Net.WebSocket.Registry.Control.Shutdown;
      AWS.Server.Shutdown (Server);
      Logging.On_Stop;
   end Stop;

end Concorde.UI.Web_UI;
