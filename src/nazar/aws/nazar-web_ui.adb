with AWS.Net.WebSocket.Registry.Control;
with AWS.Server;
with AWS.Status;

with Nazar.Web_UI.Handlers;
with Nazar.Web_UI.Logging;
with Nazar.Web_UI.Routes;

with Nazar.Json;

package body Nazar.Web_UI is

   type Socket_Type is
     new AWS.Net.WebSocket.Object
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

   procedure Send_Message
     (Socket     : in out Socket_Type'Class;
      Message    : Nazar.Json.Json_Value'Class);

   function Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data)
      return AWS.Net.WebSocket.Object'Class;

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

   procedure Send_Message
     (Socket     : in out Socket_Type'Class;
      Message    : Nazar.Json.Json_Value'Class)
   is
   begin
      Socket.Send
        (Message => Message.Serialize);
   end Send_Message;

   -----------
   -- Start --
   -----------

   procedure Start
     (Application_Name : String;
      Port             : Natural)
   is
   begin

      Logging.On_Starting;

      Create_Routes;
      Create_Socket;

      AWS.Server.Start
        (Web_Server => Server,
         Name       => Application_Name,
         Callback   => Routes.Handle_Http_Request'Access,
         Port       => Port);

   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop
     (Message : String)
   is
   begin
      Logging.On_Stopping (Message);
      AWS.Net.WebSocket.Registry.Control.Shutdown;
      AWS.Server.Shutdown (Server);
      Logging.On_Stop;
   end Stop;

end Nazar.Web_UI;
