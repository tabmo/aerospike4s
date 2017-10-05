function filterByAge(stream, min)
  local function person(record)
    local t = map()
    t.name = record.name
    t.age = record.age
    return t
  end

  local function ageMin(record)
    return record.age >= min
  end

  return stream : filter(ageMin) : map(person)
end
