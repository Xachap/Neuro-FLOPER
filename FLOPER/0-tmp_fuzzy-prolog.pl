node_1_2(X1,X2,X3,X4,TV0) :- td(X2,_TV1), lat:agr_prod(0.7056782, _TV1, _TV2), td(X4,_TV3), lat:agr_prod(-0.38496542, _TV3, _TV4), td(X1,_TV5), lat:agr_prod(0.7310477, _TV5, _TV6), td(X3,_TV7), lat:agr_prod(-0.7602609, _TV7, _TV8), lat:agr_add(_TV6, _TV8, _TV9), lat:agr_add(_TV4, _TV9, _TV10), lat:agr_add(_TV2, _TV10, _TV11), lat:agr_add(0.6244366, _TV11, _TV12), lat:sym_agr_s1(_TV12, TV0).
node_2_2(X1,X2,X3,X4,TV0) :- td(X2,_TV1), lat:agr_prod(0.3289035, _TV1, _TV2), td(X4,_TV3), lat:agr_prod(0.9662396, _TV3, _TV4), td(X1,_TV5), lat:agr_prod(-0.5254206, _TV5, _TV6), td(X3,_TV7), lat:agr_prod(0.6307893, _TV7, _TV8), lat:agr_add(_TV6, _TV8, _TV9), lat:agr_add(_TV4, _TV9, _TV10), lat:agr_add(_TV2, _TV10, _TV11), lat:agr_add(-0.5086457, _TV11, _TV12), lat:sym_agr_s1(_TV12, TV0).
node_3_2(X1,X2,X3,X4,TV0) :- td(X2,_TV1), lat:agr_prod(0.9592636, _TV1, _TV2), td(X4,_TV3), lat:agr_prod(-0.8759228, _TV3, _TV4), td(X1,_TV5), lat:agr_prod(0.14646693, _TV5, _TV6), td(X3,_TV7), lat:agr_prod(-0.35557893, _TV7, _TV8), lat:agr_add(_TV6, _TV8, _TV9), lat:agr_add(_TV4, _TV9, _TV10), lat:agr_add(_TV2, _TV10, _TV11), lat:agr_add(0.8460643, _TV11, _TV12), lat:sym_agr_s1(_TV12, TV0).
node_4_2(X1,X2,X3,X4,TV0) :- td(X2,_TV1), lat:agr_prod(0.4458459, _TV1, _TV2), td(X4,_TV3), lat:agr_prod(-0.4348098, _TV3, _TV4), td(X1,_TV5), lat:agr_prod(-0.39125273, _TV5, _TV6), td(X3,_TV7), lat:agr_prod(-0.31539902, _TV7, _TV8), lat:agr_add(_TV6, _TV8, _TV9), lat:agr_add(_TV4, _TV9, _TV10), lat:agr_add(_TV2, _TV10, _TV11), lat:agr_add('#sym'(n1), _TV11, _TV12), lat:sym_agr_s1(_TV12, TV0).
node_5_2(X1,X2,X3,X4,TV0) :- td(X2,_TV1), lat:agr_prod(-0.7359405, _TV1, _TV2), td(X4,_TV3), lat:agr_prod(1.3490844, _TV3, _TV4), td(X1,_TV5), lat:agr_prod(0.33556515, _TV5, _TV6), td(X3,_TV7), lat:agr_prod(0.22204085, _TV7, _TV8), lat:agr_add(_TV6, _TV8, _TV9), lat:agr_add(_TV4, _TV9, _TV10), lat:agr_add(_TV2, _TV10, _TV11), lat:agr_add(-0.47975427, _TV11, _TV12), lat:sym_agr_s1(_TV12, TV0).
node_6_2(X1,X2,X3,X4,TV0) :- td(X2,_TV1), lat:agr_prod(0.76345056, _TV1, _TV2), td(X4,_TV3), lat:agr_prod(1.7279333, _TV3, _TV4), td(X1,_TV5), lat:agr_prod(0.3039163, _TV5, _TV6), td(X3,_TV7), lat:agr_prod(1.1799886, _TV7, _TV8), lat:agr_add(_TV6, _TV8, _TV9), lat:agr_add(_TV4, _TV9, _TV10), lat:agr_add(_TV2, _TV10, _TV11), lat:agr_add(0.41351318, _TV11, _TV12), lat:sym_agr_s1(_TV12, TV0).
node_7_2(X1,X2,X3,X4,TV0) :- td(X2,_TV1), lat:agr_prod(-0.6056487, _TV1, _TV2), td(X4,_TV3), lat:agr_prod(0.04486853, _TV3, _TV4), td(X1,_TV5), lat:agr_prod(-0.26603696, _TV5, _TV6), td(X3,_TV7), lat:agr_prod(-0.1796673, _TV7, _TV8), lat:agr_add(_TV6, _TV8, _TV9), lat:agr_add(_TV4, _TV9, _TV10), lat:agr_add(_TV2, _TV10, _TV11), lat:agr_add(0.0, _TV11, _TV12), lat:sym_agr_s1(_TV12, TV0).
node_8_2(X1,X2,X3,X4,TV0) :- td(X2,_TV1), lat:agr_prod(1.0807129, _TV1, _TV2), td(X4,_TV3), lat:agr_prod(-0.3655969, _TV3, _TV4), td(X1,_TV5), lat:agr_prod(0.03292477, _TV5, _TV6), td(X3,_TV7), lat:agr_prod(-0.82385653, _TV7, _TV8), lat:agr_add(_TV6, _TV8, _TV9), lat:agr_add(_TV4, _TV9, _TV10), lat:agr_add(_TV2, _TV10, _TV11), lat:agr_add(0.3956585, _TV11, _TV12), lat:sym_agr_s1(_TV12, TV0).
iris_setosa(X1,X2,X3,X4,TV0) :- node_3_2(X1,X2,X3,X4,_TV1), lat:agr_prod(1.4839547, _TV1, _TV2), node_2_2(X1,X2,X3,X4,_TV3), lat:agr_prod(-1.1248782, _TV3, _TV4), node_1_2(X1,X2,X3,X4,_TV5), lat:agr_prod(0.5240745, _TV5, _TV6), node_4_2(X1,X2,X3,X4,_TV7), lat:agr_prod(-0.5511421, _TV7, _TV8), node_7_2(X1,X2,X3,X4,_TV9), lat:agr_prod(0.17743558, _TV9, _TV10), node_5_2(X1,X2,X3,X4,_TV11), lat:agr_prod(-1.555479, _TV11, _TV12), node_8_2(X1,X2,X3,X4,_TV13), lat:agr_prod(0.3185118, _TV13, _TV14), node_6_2(X1,X2,X3,X4,_TV15), lat:agr_prod(-1.1771655, _TV15, _TV16), lat:agr_add(_TV14, _TV16, _TV17), lat:agr_add(_TV12, _TV17, _TV18), lat:agr_add(_TV10, _TV18, _TV19), lat:agr_add(_TV8, _TV19, _TV20), lat:agr_add(_TV6, _TV20, _TV21), lat:agr_add(_TV4, _TV21, _TV22), lat:agr_add(_TV2, _TV22, _TV23), lat:agr_add(-0.011161028, _TV23, _TV24), lat:sym_agr_s2(_TV24, TV0).
iris_versicolor(X1,X2,X3,X4,TV0) :- node_3_2(X1,X2,X3,X4,_TV1), lat:agr_prod(0.48719922, _TV1, _TV2), node_2_2(X1,X2,X3,X4,_TV3), lat:agr_prod(-0.9323984, _TV3, _TV4), node_1_2(X1,X2,X3,X4,_TV5), lat:agr_prod(0.3396025, _TV5, _TV6), node_4_2(X1,X2,X3,X4,_TV7), lat:agr_prod(0.68769246, _TV7, _TV8), node_7_2(X1,X2,X3,X4,_TV9), lat:agr_prod(0.2782907, _TV9, _TV10), node_5_2(X1,X2,X3,X4,_TV11), lat:agr_prod(0.050164532, _TV11, _TV12), node_8_2(X1,X2,X3,X4,_TV13), lat:agr_prod(-2.845827, _TV13, _TV14), node_6_2(X1,X2,X3,X4,_TV15), lat:agr_prod(-0.471039, _TV15, _TV16), lat:agr_add(_TV14, _TV16, _TV17), lat:agr_add(_TV12, _TV17, _TV18), lat:agr_add(_TV10, _TV18, _TV19), lat:agr_add(_TV8, _TV19, _TV20), lat:agr_add(_TV6, _TV20, _TV21), lat:agr_add(_TV4, _TV21, _TV22), lat:agr_add(_TV2, _TV22, _TV23), lat:agr_add(0.008997908, _TV23, _TV24), lat:sym_agr_s2(_TV24, TV0).
iris_virginica(X1,X2,X3,X4,TV0) :- node_3_2(X1,X2,X3,X4,_TV1), lat:agr_prod(-1.8248173, _TV1, _TV2), node_2_2(X1,X2,X3,X4,_TV3), lat:agr_prod(1.0094665, _TV3, _TV4), node_1_2(X1,X2,X3,X4,_TV5), lat:agr_prod(-1.764419, _TV5, _TV6), node_4_2(X1,X2,X3,X4,_TV7), lat:agr_prod(0.47558457, _TV7, _TV8), node_7_2(X1,X2,X3,X4,_TV9), lat:agr_prod(-0.622447, _TV9, _TV10), node_5_2(X1,X2,X3,X4,_TV11), lat:agr_prod(0.8063772, _TV11, _TV12), node_8_2(X1,X2,X3,X4,_TV13), lat:agr_prod(0.19881459, _TV13, _TV14), node_6_2(X1,X2,X3,X4,_TV15), lat:agr_prod(-0.15260419, _TV15, _TV16), lat:agr_add(_TV14, _TV16, _TV17), lat:agr_add(_TV12, _TV17, _TV18), lat:agr_add(_TV10, _TV18, _TV19), lat:agr_add(_TV8, _TV19, _TV20), lat:agr_add(_TV6, _TV20, _TV21), lat:agr_add(_TV4, _TV21, _TV22), lat:agr_add(_TV2, _TV22, _TV23), lat:agr_add(-0.7190468, _TV23, _TV24), lat:sym_agr_s2(_TV24, TV0).