/^[[:space:]]*DNS[[:space:]]*=/ {
  next
}

/^[[:space:]]*(Address|AllowedIPs)[[:space:]]*=/ {
  separator_index = index($0, "=")
  key = substr($0, 1, separator_index)
  value_count = split(substr($0, separator_index + 1), values, ",")
  output = key
  separator = " "

  for (index_value = 1; index_value <= value_count; index_value++) {
    value = values[index_value]
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", value)

    if (index(value, ":") == 0) {
      output = output separator value
      separator = ", "
    }
  }

  print output
  next
}

{ print }
