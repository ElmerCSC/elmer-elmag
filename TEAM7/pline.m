load res/f.dat
time=10;
line=1;
bzi=22;
xi=5;
tmax=max(f(:,1));

#A1-B1 B-field data
bz_team =[-0.00049 -0.001788 -0.002213 -0.002019 -0.001567 ...
0.000036 0.004364 0.007811 0.007155 0.006044 0.005391  ...
0.005262 0.005381 0.005691 0.005924 0.005278 0.002761];

x_team = [0 0.018 0.036 0.054 0.072 0.09 0.108 0.126 0.144  ...
0.162 0.18 0.198 0.216 0.234 0.252 0.27 0.288];

hold off
for time=1:tmax
  ind=find(f(:,1)==time & f(:,3)==line);
  x=f(ind,xi);
  [px perm]=sort(x);
  
  # if interested in plotting all timesteps 
  #uncomment below and comment last timestep plot
  #plot(px,f(ind(perm),bzi));
  #pause(0.2);
  #hold on;
endfor
 
# last time-step (peak value)
# Elmer Fem resutls
plot(px,f(ind(perm),22));
pause(0.2);
hold on;

#TEAM7 data (A1-B1)
plot(x_team,bz_team,'r');
pause(0.2);
hold on;

title('Bz along line A1-B1');
xlabel('x(m)');
ylabel('Bz (T)');
legend ('Elmer', 'Team7')



