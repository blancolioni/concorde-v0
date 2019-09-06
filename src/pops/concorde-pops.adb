with Concorde.Db.Pop;

package body Concorde.Pops is

   -----------
   -- Happy --
   -----------

   function Happy
     (Pop : Concorde.Db.Pop_Reference)
      return Non_Negative_Real
   is
   begin
      return Concorde.Db.Pop.Get (Pop).Happy;
   end Happy;

   ------------
   -- Health --
   ------------

   function Health
     (Pop : Concorde.Db.Pop_Reference)
      return Non_Negative_Real
   is
   begin
      return Concorde.Db.Pop.Get (Pop).Health;
   end Health;

   -----------
   -- Hours --
   -----------

   function Hours
     (Pop : Concorde.Db.Pop_Reference)
      return Non_Negative_Real
   is
   begin
      return Concorde.Db.Pop.Get (Pop).Hours;
   end Hours;

end Concorde.Pops;
