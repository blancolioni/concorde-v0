with Concorde.Handles.Pop;

package body Concorde.Pops is

   -----------
   -- Happy --
   -----------

   function Happy
     (Pop : Concorde.Handles.Pop_Reference)
      return Non_Negative_Real
   is
   begin
      return Concorde.Handles.Pop.Get (Pop).Happy;
   end Happy;

   ------------
   -- Health --
   ------------

   function Health
     (Pop : Concorde.Handles.Pop_Reference)
      return Non_Negative_Real
   is
   begin
      return Concorde.Handles.Pop.Get (Pop).Health;
   end Health;

   -----------
   -- Hours --
   -----------

   function Hours
     (Pop : Concorde.Handles.Pop_Reference)
      return Non_Negative_Real
   is
   begin
      return Concorde.Handles.Pop.Get (Pop).Hours;
   end Hours;

end Concorde.Pops;
