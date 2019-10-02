with AWS.Server;
with AWS.Status;

with Concorde.UI.Web_UI.Handlers;
with Concorde.UI.Web_UI.Routes;

package body Concorde.UI.Web_UI is

   procedure Create_Routes;

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
   end Create_Routes;

   -----------
   -- Start --
   -----------

   overriding procedure Start (Web_UI : in out Web_UI_Type) is
      pragma Unreferenced (Web_UI);
      WS : AWS.Server.HTTP;
   begin
      Create_Routes;

      AWS.Server.Start
        (Web_Server => WS,
         Name       => "Concorde",
         Callback   => Routes.Handle'Access,
         Port       => 8080);
      AWS.Server.Wait;
   end Start;

end Concorde.UI.Web_UI;
