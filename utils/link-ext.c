#include "link-includes.h"

int main(int argc, char *argv[])
{
  Dictionary    dict;
  Parse_Options opts;
  Sentence      sent;
  Linkage       linkage;
  char *        diagram;
  int           i, num_linkages;

  fprintf(stderr, "Processing: %s\n", argv[1]);
  opts  = parse_options_create();
  dict  = dictionary_create("/opt/chatyeo/data/link-data/4.0.dict", "/opt/chatyeo/data/link-data/4.0.knowledge", NULL, "/opt/chatyeo/data/link-data/4.0.affix");

  sent = sentence_create(argv[1], dict);
  num_linkages = sentence_parse(sent, opts);
  if (num_linkages > 0)
  {
    linkage = linkage_create(0, sent, opts);
    fprintf(stdout, "%s\n", diagram = linkage_print_diagram(linkage));
    string_delete(diagram);
    linkage_delete(linkage);
  }
  sentence_delete(sent);

  dictionary_delete(dict);
  parse_options_delete(opts);
  return 0;
}
