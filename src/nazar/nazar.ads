package Nazar is

   type Nazar_Float is new Long_Float range
     Long_Float'First .. Long_Float'Last;

   type Measure is new Nazar_Float range 0.0 .. Nazar_Float'Last;

end Nazar;
