with Reiko;

package Concorde.Updates is

   type Root_Update_Type is
     abstract new Reiko.Root_Update_Type with private;

private

   type Root_Update_Type is
     abstract new Reiko.Root_Update_Type with
      record
         null;
      end record;

end Concorde.Updates;
