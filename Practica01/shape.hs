module Shape where

data Shape=Circle Float
          |Square Float
          |Rectangle Float Float
          |Triangle Float Float
          |Trapeze Shape Shape
          deriving(Show)


area::Shape->Float
area (Circle r)=pi^2 * r
area (Square l) = l^2
area (Rectangle b h) = b * h
area (Triangle b h) = (b*h)/2
area (Trapeze a b)=2*(area a) + area b

perimeter::Shape->Float
perimeter (Circle r)=pi*(2*r)
perimeter (Square l)=4*l
perimeter (Rectangle b h)=2*b+2*h
perimeter (Triangle b h)=b+h+hipotenusa(Triangle b h)
perimeter (Trapeze a b)=2*hipotenusa(a)+dobleBase(a)+dobleBase(b)


-- Metodos auxiliares
hipotenusa::Shape->Float
hipotenusa (Triangle b h) =sqrt(b^2+h^2)

dobleBase::Shape->Float
dobleBase (Rectangle b h)=2*b
dobleBase (Triangle b h)=2*b

-- Variables de prueba
sq1=Square 4
rc1=Rectangle 6 3
t1=Triangle 3 5
tp1=Trapeze t1 rc1








