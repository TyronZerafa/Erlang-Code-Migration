{application, remote_evaluation,
 [{description, "The Code Migration and Remote Evaluation application metadata file"},
  {vsn, "0.1.0"},
  {modules, [ 
  	closure_rep,
  	evaluate,
  	general_utils,
  	mcode,
  	mcode_cb,
  	mcode_supervisor,
  	pf_gf,
  	pf_lexer,
  	pr_rules_parser,
  	remote_evaluation,
  	resource_server	
  ]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {remote_evaluation, []}}
 ]}.
