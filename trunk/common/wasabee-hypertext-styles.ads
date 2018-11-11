private package Wasabee.Hypertext.Styles is

  --  Set a property on a local style (e.g. on an hypertext node)
  --  val is zero or more arguments separated by space(s).
  procedure Set_property(sty: in out Local_Style; key, vals: String);

  --  Parse the contents of "style='...'" attribute and apply them directly to
  --  a local style that will be merged later during a call to Apply_style.
  procedure Parse_immediate_style(sty: in out Local_Style; v: String);

  --  Apply and merge styles from parent style, CSS rules and
  --  local style to a node and its descendents. If the node is a top node, it
  --  is meaningful to have the main/initial style as parent_style.
  procedure Apply_styles(
    node         : p_Body_Node;
    css_map      : CSS.CSS_Dictionary.Map;
    parent_style : Local_Style
  );

  --  Initialize_styles sets an initial style sheet (CSS).
  --  Rules from the web page may replace those initial rules.
  --  !! Should be configurable with a few default fonts (cf Chrome) as parameters
  procedure Initialize_styles(ho: in out HT_object);

end Wasabee.Hypertext.Styles;
