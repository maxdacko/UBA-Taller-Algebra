f x y = x*x+y*y
g x y z = x+y+z*z
doble x = 2*x
normaVectorial x1 x2 = sqrt (x1*x1+x2*x2)

h n | n == 0 = 1
	| otherwise = 0
	
signo n | n>0= 1
		| n==0= 0
		| n<0= -1
		
maximo x y | x>=y = x
		   | otherwise = y 
		   
cantSolu b c |b^2-4*c>0 = 2
			 |b^2-4*c==0 = 1
			 | otherwise = 0
			 
esPar :: Int -> Bool
esPar n | mod n 2 == 0 = True
		| otherwise = False
		
absoluto x | x>=0 = x
		   | x<0 = (-x)	
		   
maximoAbs x y | e> (sqrt(y^2)) = x
			  | e< (sqrt(y^2)) = y 
			  where e = (sqrt(x^2)) 
			  
maximo3 x y z = max x (max y z) 


			
			  