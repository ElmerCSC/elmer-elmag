load res/f.dat
time=10;
line=1;
bzi=22;
xi=5;
tmax=max(f(:,1));

hold off
for time=1:tmax
  ind=find(f(:,1)==time & f(:,3)==line);
  x=f(ind,xi);
  [px perm]=sort(x);
  plot(px,f(ind(perm),bzi));
  pause(0.2);
  hold on;
endfor
 
title('Bz along line A1-B1');
xlabel('x(m)');
ylabel('Bz');
