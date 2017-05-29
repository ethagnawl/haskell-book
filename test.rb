
def how_many_divisors?(number)
original=number
x=2
divisor=2           #I am including 1 and itself here.
until x>number
    until number%x!=0
        number/=x
        divisor+=1
    end
    if x== original    #This is needed to correct for prime numbers which would count 1 and itself twice
        divisor-=1
    end
    x+=1
end
return divisor
end

x=2
triangle =3
divisors=0
until divisors>500
    x+=1
    triangle+=x
    divisors=how_many_divisors?(triangle)
end
puts "The first triangle number to have more than 500 divisors is #{triangle}"


puts "Press enter to exit."  #Stops script incase I'm running directly from folder.
quit = gets.chomp
