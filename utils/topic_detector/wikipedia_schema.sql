CREATE TABLE page (
	page_id int(8) unsigned NOT NULL,
	page_title varchar(255) binary NOT NULL,
	page_type int(2) NOT NULL default '0',
	PRIMARY KEY  (page_id),
	UNIQUE KEY type_title (page_type,page_title)) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE redirect ( 
	rd_from int(8) unsigned NOT NULL,
	rd_to int(8) unsigned NOT NULL,
	PRIMARY KEY (rd_from, rd_to),
	KEY rd_to (rd_to)) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE categorylink ( 
	cl_parent int(8) unsigned NOT NULL,
	cl_child int(8) unsigned NOT NULL,
	PRIMARY KEY (cl_parent, cl_child),
	KEY cl_child (cl_child)) ENGINE=MyISAM DEFAULT CHARSET=utf8;		  

CREATE TABLE translation ( 
	tl_id int(8) unsigned NOT NULL,
	tl_lang varchar(10) binary NOT NULL,
	tl_text varchar(255) binary NOT NULL,
	PRIMARY KEY (tl_id, tl_lang)) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE disambiguation ( 
	da_from int(8) unsigned NOT NULL,
	da_to int(8) unsigned NOT NULL,
	da_index int(3) unsigned NOT NULL,
	da_scope mediumblob NOT NULL,
	PRIMARY KEY (da_from, da_to),
	KEY da_to (da_to)) ENGINE=MyISAM DEFAULT CHARSET=utf8;	 

CREATE TABLE linkcount ( 
	lc_id int(8) unsigned NOT NULL,
	lc_in int(8) unsigned NOT NULL,
	lc_out int(8) unsigned NOT NULL,
	PRIMARY KEY (lc_id)) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE content ( 
	co_id int(8) unsigned NOT NULL,
	co_content mediumblob NOT NULL,
	PRIMARY KEY (co_id)) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE pagelink_in ( 
	li_id int(8) unsigned NOT NULL,
	li_data mediumblob NOT NULL,
	PRIMARY KEY (li_id)) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE pagelink_out ( 
	lo_id int(8) unsigned NOT NULL,
	lo_data mediumblob NOT NULL,
	PRIMARY KEY (lo_id)) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE equivalence ( 
	eq_cat int(8) unsigned NOT NULL,
	eq_art int(8) unsigned NOT NULL,
	PRIMARY KEY (eq_cat), 
	UNIQUE KEY (eq_art)) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE anchor ( 
	an_text varchar(300) binary NOT NULL,
	an_to int(8) unsigned NOT NULL,
	an_count int(8) unsigned NOT NULL,
	PRIMARY KEY (an_text, an_to), 
	KEY (an_to)) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE anchor_occurance ( 
	ao_text varchar(300) binary NOT NULL,
	ao_linkCount int(8) unsigned NOT NULL,
	ao_occCount int(8) unsigned NOT NULL,
	PRIMARY KEY (ao_text)) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE stats ( 
	st_articles int(8) unsigned NOT NULL,
	st_categories int(8) unsigned NOT NULL,
	st_redirects int(8) unsigned NOT NULL,
	st_disambigs int(8) unsigned NOT NULL) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE generality ( 
	gn_id int(8) unsigned NOT NULL,
	gn_depth int(2) unsigned NOT NULL,
	PRIMARY KEY (gn_id)) ENGINE=MyISAM DEFAULT CHARSET=utf8;

