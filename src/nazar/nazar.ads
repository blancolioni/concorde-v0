with WL.Guids;

package Nazar is

   type Nazar_Float is new Long_Float range
     Long_Float'First .. Long_Float'Last;

   type Nazar_Unit_Float is new Nazar_Float range 0.0 .. 1.0;

   type Nazar_Object_Interface is interface;

   function Guid (Object : Nazar_Object_Interface)
                  return WL.Guids.Guid
                  is abstract;

end Nazar;
