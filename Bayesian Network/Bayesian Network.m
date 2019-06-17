%test_BNT
%Yueyue April Sang 

%Graph structure
N = 5; 
dag = zeros(N,N);
E = 1; D = 2; LW = 3; BNC = 4; AOC = 5;
dag([E,D],LW) = 1;
dag(LW,BNC) = 1;
dag(LW,AOC)=1;

%Creating the Bayes net shell
discrete_nodes = 1:N;
node_sizes = 2*ones(1,N); 

bnet = mk_bnet(dag, node_sizes, 'discrete', discrete_nodes);

onodes = [];
bnet = mk_bnet(dag, node_sizes, 'discrete', discrete_nodes);
bnet = mk_bnet(dag, node_sizes, 'names', {'E','D','LW','BNC','AOC'}, 'discrete', 1:5);

%Parameters
%C = bnet.names{'cloudy'}; bnet.names is an associative array
bnet.CPD{E} = tabular_CPD(bnet, E, [0.5 0.5]);
bnet.CPD{D} = tabular_CPD(bnet, D, [0.5 0.5]);

bnet.CPD{LW} = tabular_CPD(bnet, LW, [0.05 0.40 0.29 0.95 0.95 0.60 0.71 0.05]);
bnet.CPD{BNC} = tabular_CPD(bnet, BNC, [0.15 0.85 0.85 0.15]);
bnet.CPD{AOC} = tabular_CPD(bnet, AOC, [0.35 0.70 0.65 0.30]);

%Inference
engine = jtree_inf_engine(bnet);

%Computing marginal distributions
evidence = cell(1,N);
evidence{E} = 2;
evidence{D} = 2;

[engine, loglik] = enter_evidence(engine, evidence);

marg = marginal_nodes(engine, BNC);
marg.T
p = marg.T(2);
%p = 0.8150

evidence = cell(1,N);
evidence{D} = 2;
evidence{E} = 1;

[engine, loglik] = enter_evidence(engine, evidence);

marg = marginal_nodes(engine, AOC);
marg.T
p2 = marg.T(1);
%p2 = 0.5985

%bar(marg.T)
%draw_graph(bnet.dag)

