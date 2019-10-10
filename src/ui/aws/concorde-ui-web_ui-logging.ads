with AWS.Response;
with AWS.Status;

private package Concorde.UI.Web_UI.Logging is

   procedure Log_Request
     (Request  : AWS.Status.Data;
      Response : AWS.Response.Data);

   procedure On_Starting;

   procedure On_Stopping (Message : String);
   procedure On_Stop;

end Concorde.UI.Web_UI.Logging;
