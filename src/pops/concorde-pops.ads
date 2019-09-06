with Concorde.Db;

package Concorde.Pops is

   function Happy
     (Pop : Concorde.Db.Pop_Reference)
      return Non_Negative_Real;

   function Hours
     (Pop : Concorde.Db.Pop_Reference)
      return Non_Negative_Real;

   function Health
     (Pop : Concorde.Db.Pop_Reference)
      return Non_Negative_Real;

end Concorde.Pops;
