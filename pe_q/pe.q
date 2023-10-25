.pe.test:{[n;arg;ans]1 "Problem ",(string n)," test:\n\t(out: ",(string (get `$".probs.p",string n) arg),") == (ans: ",(string ans),")?\n\n";}
.pe.range:{$[z>0;c:>;c:<]; c[y-z;] +[z;] \x}
.pe.isqrt:{"j"$ sqrt x}
.pe.cwd:":/Users/boneham/project_euler/pe_q/"
.pe.gcd:{$[y=0;x;.z.s[y;x mod y]]}
.pe.eratosthenes:{[n] mask:0011b,((n-3)#010100b);imax:.pe.isqrt n;i:5;
 while[i<=imax;mask[.pe.range[i*i;n;i]]:0b;while[not mask[i+:2];]];
 where mask}

.probs.p1:{[n](+/){[m;x](+/)x*1+til floor m% abs x}[n-1;]'[3 5 -15]}

.probs.p2:{[n]f:0 2;while[n>=g:((+/)(-2#f)*1 4);f,:g];(+/)f}

.probs.p3:{[n]i:2;while[i<n;n:{while[0=x mod y;x:x div y];x}[n;i];i+:1];i}

.probs.p4:{[]
 .p4.pals:(,/){(,/){(.pe.range[9900;-1;-1100])+'x}'[(.pe.range[90090;-1;-10010])+'x]}'[.pe.range[900009;0;-100001]];
 .p4.pals[{p:.p4.pals[x];not(|/)(0=)mod[p;.pe.range[div[p;999];.pe.isqrt[p];1]]} +[1;]/ 1]}

.probs.p5:{[n]({div[x*y;.pe.gcd[x;y]]}/) 1+til n}

.probs.p6:{[n]{(2*x[0])-(+/)x}{x*x}'[{((+/)x),x}1+til n]}

.probs.p7:{[n](.pe.eratosthenes (ceiling n*log[n*log[n]]))[n]}

.probs.p8:{[n]nums:eval parse (read0 `$.pe.cwd,"p8.txt")[0]; (|/)({(*/)z[x+til y]}[;n;nums]') til 1001-n}

.probs.p9:{[]m:where{0=mod[500;x]}til 251;
 n:{a:.pe.range[1+mod[x;2];x;2];x,first a[where 0=mod[500;x*x+a]]};
 mn:last ((n') m)[where ((last') ((n') m))<>0N];
 {(500 div x[0]*x[0]+x[1])*2*x[0]*x[1]*((-/)2{x*x}/x)}mn}

.probs.p10:{(+/).pe.eratosthenes x}

.probs.p11:{[]grid:1_parse ({x,";",y}/)(read0 `$.pe.cwd,"p11.txt");
 (|/)({[x;k]i:k[0];j:k[1];r:x[i];c:x[;j];
   (|/)(*/')(c[i],c[i+1],c[i+2],c[i+3];r[j],r[j+1],r[j+2],r[j+3];
    x[i;j],x[i+1;j+1],x[i+2;j+2],x[i+3;j+3];x[i;j],x[i+1;j-1],x[i+2;j-2],x[i+3;j-3])}[grid]')
 ((til 20)cross(til 20))}

.probs.p12:{[n]primes:.pe.eratosthenes 15000;
 sig:{[n](*/){[a;x;p]$[0=mod[x;p];.z.s[a+1;div[x;p];p];a]}[1;n] each primes[til {x>=primes[y]}[n;]+[1]/0]};
 set[`P12CACHE;(0 1)!(1 1)];
 sigm:{$[not null c:P12CACHE x;c;P12CACHE[x]:sig x]};
 sigT:{$[0=mod[x;2];y[div[x;2]]*y[x+1];y[div[x+1;2]]*y[x]]}[;sigm];
 out:{while[y>sigT x+:1];div[x*(x+1);2]}[1;n];
 delete P12CACHE from `.;
 out}

.probs.p13:{[]nums:parse each{x," ",y}/'[read0 `$.pe.cwd,"p13.txt"]; clmns:{+/[y[;x]]}[;nums]'[til 50]; i:49;
 while[i>0;clmns[i-1]+::div[clmns[i];10];clmns[i]::mod[clmns[i];10];i-:1];
 parse (""{x,string y}/ clmns)[til 10]}

.probs.p14:{[n]
 .p14.n:n;
 .p14.c:1 1,(n-1)#0;
 .p14.f:{$[.p14.c[x]>0;enlist x;.z.s[$[0=mod[x;2];div[x;2];1+3*x]],x]};
 .p14.g:{-[;1]{.p14.c[x[0]]{$[y<=.p14.n;1+.p14.c[y]:x;1+x]}/x}[.p14.f x]};
 ((1 1){$[.p14.c[y]=0;$[x[1]<v:.p14.g y;(y,v); x];x]}/reverse div[n+1;2] + til (1+div[n+1;2]))[0]}

.probs.p15:{[n].p15.f:reverse n{x,enlist ,[y#0;1]}[;n]/(enlist (n+1)#1);
 1{x|.p15.f[y[0];y[1]]:.p15.f[y[0]+1;y[1]]+.p15.f[y[0];y[1]+1]}/{x cross x} reverse til n}

.probs.p16:{[n].p16.c:floor(n*log 2)%(log 10);
 .p16.i:til .p16.c;
 a:1,.p16.c#0;
 (+/)n {{+[0,div[x[.p16.i];10];mod[x[.p16.i];10],x[.p16.c]]}[x*2]}/ a}

.probs.p18:{[]@[{y+|[1_x;-1_x]}/;reverse(parse')read0 `$.pe.cwd,"p18.txt"][0]}

.probs.p19:{[]{while[2000.12.31>x+:7;if[1=`dd$x;y+:1]];y}[1900.01.07+7*div[365;7];0]}

.probs.p20:{[n]
 .p20.c:{ceiling %[*[x;-[log x;log 2]];log 10]}[n];
 .p20.i:til .p20.c;
 a:1,.p20.c#0;
 sum a{@[{+[0,div[x[.p20.i];10];mod[x[.p20.i];10],x[.p20.c]]}/;x*y]}/1+til n}

.probs.p21:{[n] .p21.u:{h:sum 1%1+til x;floor h+exp[h]*log[h]}[n]; .p21.d:.p21.u#1;
 (div[.p21.u;2]>) {.p21.d[.pe.range[y+y;x;y]]+:y;y+1}[.p21.u;]/ 2;
 @[+/;i where {&[x<>a;x=.p21.d[a:.p21.d[x]]]} each i:1+til n]}

.probs.p22:{[]names:asc[({1_-1_x}')vs[",";(read0 `$.pe.cwd,"p22.txt")[0]]];
 {@[+/;*[1+til count x;{@[+/;({-[;64]"j"$x}')x]} each x]]}[names]}

.probs.p23:{[n]u:{h:sum 1%1+til x;2*floor h+exp[h]*log[h]}[n];
 .p23.d:u#1;
 (div[u;2]>) {.p23.d[.pe.range[y+y;x;y]]+:y;y+1}[u;]/ 2;
 a:i where {x<.p23.d[x]} each i:1+til n;
 .p23.b:n#1b;
 m:count a;
 while[m-:1;a:{.p23.b[(x[0]+x[where y>x[0]+x])]:0b; 1_x}[a;n]];
 (+/)where .p23.b}

.probs.p24:{[n]
 {$[y=0;z{y+10*x}/x;
   .z.s[#[i;x],_[i+1;x];mod[y;j];+[10*z;x[i:div[y;j:@[*/;1+til -[;1] count x]]]]]]}
 [til 10;n-1;0]}

.probs.p25:{[n].p25.i:1+til n-1; f0:(((n-1)#0),1;((n-1)#0),1); c:2;
 while[0=(f0:{(x[1];+[div[x1[.p25.i];10],0;x1[0],mod[(x1:x[0]+x[1])[.p25.i];10]])} f0)[0;0];c+:1];c}

.probs.p26:{[n]
 f:{q:r:0#q1:-[;1]r1:1;
  while[not(|/)&[q1=q;r1=r]; q,:q1; r,:r1; q1:div[10*r1;x]; r1:mod[10*r1;x]];
  1+?[reverse &[q1=q;r1=r];1b]};
 m:0; while[n>m:max[m,f (n-:1)]]; n+1}

.probs.p27:{[]
 p:.pe.eratosthenes {997+x*x+999} 80;
 .p27.s:{b:-[;1]count x+a:0; while[(b-1)>a:$[x[b:$[x[a]=y;a;b]]=y;b;a];$[x[c:div[a+b;2]]>y;b:c;a:c]]; x[a]=y}[p]';
 f:{[b]{[b;n;a;a0]$[0>=count(a:a[where .p27.s[b+n*a+n]]);n,b*a0;.z.s[b;n+1;a;a[0]]]}[b;0;.pe.range[2-b;1000;2];0]};
 {m:max[x][0];x[where {x[0]=y}[;m] each x][0;1]}[f each p[til 1+p?997]]}

while[1b;
 s:{1 x;parse (read0 0)}"Enter problem number:\n>>> ";
 $[-7h<>type s; {1 "Problem number must be a positive integer\n\n";exit x}[1];
  s=1;.pe.test[1;1000;233168];
  s=2;.pe.test[2;4000000;4613732];
  s=3;.pe.test[3;600851475143;6857];
  s=4;.pe.test[4;();906609];
  s=5;.pe.test[5;20;232792560];
  s=6;.pe.test[6;100;25164150];
  s=7;.pe.test[7;10001;104743];
  s=8;.pe.test[8;13;23514624000];
  s=9;.pe.test[9;(::);31875000];
  s=10;.pe.test[10;2000000;142913828922];
  s=11;.pe.test[11;(::);70600674];
  s=12;.pe.test[12;500;76576500];
  s=13;.pe.test[13;(::);5537376230];
  s=14;.pe.test[14;1000000;837799];
  s=15;.pe.test[15;20;137846528820];
  s=16;.pe.test[16;1000;1366];
  s=18;.pe.test[18;(::);1074];
  s=19;.pe.test[19;(::);171];
  s=20;.pe.test[20;100;648];
  s=21;.pe.test[21;10000;31626];
  s=22;.pe.test[22;(::);871198282];
  s=23;.pe.test[23;28123;4179871];
  s=24;.pe.test[24;1000000;2783915460];
  s=25;.pe.test[25;1000;4782];
  s=26;.pe.test[26;1000;983];
  s=27;.pe.test[27;(::);-59231];
  s<1; {1 "\nExiting...\n";exit x}[0];
  1b; 1 "Problem not solved yet. "]]

exit 1;
