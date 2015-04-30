-record(attribute_type, {
    real_name,
    name,
    single_value = false,
    equality
    }).

-record(object_class, {
    name,
    must_attributes,
    may_attributes
}).

