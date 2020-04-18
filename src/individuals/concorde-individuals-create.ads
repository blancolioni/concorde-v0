with Concorde.Calendar;

with Concorde.Db;

package Concorde.Individuals.Create is

   procedure New_Individual
     (Home       : Concorde.Db.Colony_Reference;
      Birth_Date : Concorde.Calendar.Time);

end Concorde.Individuals.Create;
