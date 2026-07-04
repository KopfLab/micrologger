# Resistance value editor for the sdds structure editor.
#
# This is µLogger-specific (Ohm-based resistance readings) and is passed to
# sddsParticle::sdds_server() as an additional value type so it does not have to
# live in sddsParticle itself.

# convert a stored resistance value (in Ohm) to the displayed value + units
resistance_value_to_input <- function(value, units) {
  if (value > 1e6) {
    list(v = value / 1e6, u = "MOhm")
  } else if (value > 1e3) {
    list(v = value / 1e3, u = "kOhm")
  } else {
    list(v = value, u = "Ohm")
  }
}

# convert the displayed value + units back to the stored value (in Ohm)
resistance_input_to_value <- function(input, units) {
  if (input$u == "MOhm") {
    input$v * 1e6
  } else if (input$u == "kOhm") {
    input$v * 1e3
  } else {
    input$v
  }
}

# display text for a stored resistance value
resistance_value_to_text <- function(value, units) {
  input <- resistance_value_to_input(value, units)
  paste(input$v, input$u)
}

# build the resistance value edit module (numeric value with Ohm/kOhm/MOhm units)
ml_value_resistance_input <- function(id) {
  sddsParticle::input_module_selectable_units(
    id = id,
    value_to_input = resistance_value_to_input,
    input_to_value = resistance_input_to_value,
    value_to_text = resistance_value_to_text,
    units_options = c("Ohm", "kOhm", "MOhm")
  )
}
