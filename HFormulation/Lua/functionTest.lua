
--function h_a(x) tau = 5.0; Delta_t= 5.0; ba = 1.0; t_1 = 1.0; t_2 = t_1+tau; t_3 = t_2+Delta_t; t_4 = t_3+tau; mu_0 = 1.256e-6; if (x <= t_1) then y = 0.0 elseif ((x > t_1) and (x <= t_2)) then y = math.tanh(2*(ba/tau)*(x-tau/2)+1)*(ba/mu_0)*((x-t_1)/(t_2-t_1)) elseif ((x > t_2) and (x <= t_3)) then y = (ba/mu_0) elseif ((x > t_3) and (x <= t_4)) then y = (ba/mu_0)*((x-t_4)/(t_3-t_4)) elseif (x > t_4) then y = 0.0 end return y end

function h_a(x) tau = 5.0; Delta_t= 5.0; ba = 1.0; a = 0.5*(tau+Delta_t); b = 2*a*ba/tau; c = 2*(tau+Delta_t); mu_0 = 1.256e-6; y = (ba/mu_0)/(1.0+math.abs((x-c)/a)^(2*b)) return y end

--function h_a(x) tau = 0.1; ba = 1.0; mu_0 = 1.256e-6; y = math.tanh(2*(ba/tau)*(x-tau/2)+1)*(ba/mu_0)*(x/tau)*math.exp(1-x/tau) return y end

aa = {}
bb = {}

x = 0
i = 1
while (x < 50.0) do
	aa[i] = x
	y = h_a(x); bb[i] = y
	x = x+0.01
	print(aa[i], bb[i])
	i = i+1
end	

print(i-1)


fp = io.open("data-lua.dat", "w")
for j = 1, (i-1) do
    fp:write( tostring(aa[j]).."\t"..tostring(bb[j]).."\n" )
end

io.close( fp )
