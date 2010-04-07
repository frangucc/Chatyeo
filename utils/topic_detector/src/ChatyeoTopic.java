public class ChatyeoTopic
{
  public static double MIN_WEIGHT_FOR_PRI = 0.3;
  public static double MIN_WEIGHT_FOR_SEC = 0.1;
  public static double MIN_CONTXT_FOR_PRI = 0.0;
  public static double MIN_CONTXT_FOR_SEC = 0.00;
  public static double MIN_REL_TO_OTHRS_FOR_PRI = 0.00;
  public static double MIN_REL_TO_OTHRS_FOR_SEC = 0.00;

  public String topic_name;
  public int topic_id;
  public double topic_weight;
  public double rel_to_context;
  public double rel_to_other;
    public double generality;
  public boolean is_macro;

  public ChatyeoTopic(String topic_name, int topic_id, double topic_weight,
      double rel_to_context, double rel_to_other, double generality, boolean isMacro)
  {
    this.topic_name = topic_name;
    this.topic_id = topic_id;
    this.topic_weight = Double.isNaN(topic_weight) ? 0 : topic_weight;
    this.rel_to_context = Double.isNaN(rel_to_context) ? 0 : rel_to_context;
    this.rel_to_other = Double.isNaN(rel_to_other) ? 0 : rel_to_other;
    this.is_macro = isMacro;
  }

  public boolean isPrimary()
  {
    return this.topic_weight > MIN_WEIGHT_FOR_PRI && this.rel_to_context > MIN_CONTXT_FOR_PRI && this.rel_to_other >
MIN_REL_TO_OTHRS_FOR_PRI;
  }

  public boolean isSecondary()
  {
    return this.topic_weight > MIN_WEIGHT_FOR_SEC &&
      this.rel_to_context > MIN_CONTXT_FOR_SEC &&
      this.rel_to_other >= MIN_REL_TO_OTHRS_FOR_SEC;
  }

  public String toString(){
    String s = "ChatyeoTopic[name=" + topic_name + ";id=" + topic_id + ";weight=" + topic_weight + ";rel_to_context=" + rel_to_context + ";rel_to_other=" + rel_to_other + ";is_macro=" + is_macro + "]";
    return s;
  }
}
