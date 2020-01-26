---
layout: post
title: AST based models for learning programs 
comments: true
redirect_from: "/2020/01/15/AST-based-models-for-learning-programs/"
permalink: AST-based-learning
---

I recently got a bit interested in machine learning on source code. The website 
[ml4code](https://ml4code.github.io/) gathers quite a few resources on the topic.
One particular interesting approach is using abstract syntax tree's as input to 
models for learning various properties of source code. The paper 
[A General Path-Based Representation for Predicting Program Properties]
(https://arxiv.org/abs/1803.09544) by Alon et al. suggests using AST paths for 
predicting various properties of programs. They also have two tools 
([code2vec](https://code2vec.org/), [code2seq](https://code2seq.org/)) that 
explore this idea even further by trying to predict function naming for obfuscated
Java functions. Check it out! It's pretty nice. In this post I try to cover on 
a highlevel the above paper.

