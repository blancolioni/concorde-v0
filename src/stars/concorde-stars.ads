with Concorde.Handles.Star;

package Concorde.Stars is

   subtype Star_Class is Concorde.Handles.Star.Star_Class;
   subtype Star_Handle is Concorde.Handles.Star.Star_Handle;

   function Solar_Masses
     (Star : Star_Class)
      return Non_Negative_Real;

   function Spectral_Type
     (Star : Star_Class)
      return String;

end Concorde.Stars;
