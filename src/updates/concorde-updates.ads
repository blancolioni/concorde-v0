package Concorde.Updates is

   type Update_Interface is interface;

   procedure Activate
     (Update : Update_Interface)
   is abstract;

end Concorde.Updates;
