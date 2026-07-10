# By comparing the output of getObjectType with this list, we can safely typecast it seems
copasi_object_types <-
  c(
    Model           = "_p_CModel",
    Compartment     = "_p_CCompartment",
    Metabolite      = "_p_CMetab",
    ModelValue      = "_p_CModelValue",
    Reaction        = "_p_CReaction",
    Parameter       = "_p_CCopasiParameter",
    Event           = "_p_CEvent",
    EventAssignment = "_p_CEventAssignment",
    Function        = "_p_CFunction"
  )

# automatically cast according to copasi_object_types
auto_cast <- function(c_object) {
  type <- copasi_object_types[c_object$getObjectType()]
  
  if (is.na(type))
    c_object
  else
    as(c_object, type)
}

# Takes CN as character and gives object
cn_to_object <- function(cn, c_datamodel, accepted_types = NULL) {
  if (inherits(cn, c("_p_CRegisteredCommonName", "_p_CCommonName"))) {
    cn_obj <- cn
  } else {
    assert_that(
      is.character(cn),
      length(cn) == 1L,
      !is.na(cn),
      msg = "`cn` has to be a non-missing character scalar or a CCommonName object."
    )
    cn_obj <- CRegisteredCommonName(cn)
  }

  c_object <- c_datamodel$getObjectFromCN(cn_obj)
  
  if (is.null(c_object))
    return()
  
  c_object <- auto_cast(c_object$getDataObject())
  
  if (!is.null(accepted_types) && !inherits(c_object, accepted_types))
    return()
  
  c_object
}

# Takes DisplayName as character and gives object
dn_to_object <- function(dn, c_datamodel, accepted_types = NULL) {
  c_object <- c_datamodel$findObjectByDisplayName(dn)
  
  if (is.null(c_object))
    return()
  
  c_object <- auto_cast(c_object$getDataObject())
  
  if (!is.null(accepted_types) && !inherits(c_object, accepted_types))
    return()
  
  c_object
}

# Takes wrapped CN "<*>" or reference DN "{*}" and gives object
# Only meant for references (for consistency)
xn_to_object <- function(xn, c_datamodel, accepted_types = NULL) {
  if (xn == "")
    return()
  
  if (stringr::str_detect(xn, "^<CN=.*>$"))
    return(cn_to_object(stringr::str_sub(xn, 2L, -2L), c_datamodel, accepted_types = accepted_types))
  
  # Presumably, all references are wrapped in either <CN> (handled before) or {DN} 
  assert_that(
    stringr::str_detect(xn, "^\\{.*\\}$"),
    msg = paste0("`", xn, "` could not be interpreted as value reference.")
  )
  
  dn <- unescape_ref(xn)
  c_obj <- dn_to_object(dn, c_datamodel, accepted_types = accepted_types)
  
  assert_that(
    !is.null(c_obj),
    msg = paste0("`", dn, "` in value reference `", xn, "` could not be  be resolved.")
  )
  
  # if dn_to_object doesn't return a reference I think its always
  # supposed to be the ValueReference
  if (c_obj$getObjectType() != "Reference")
    c_obj <- c_obj$getValueReference()
  
  c_obj
}

# get the CN of an object as string
get_cn <- function(c_object) {
  if (is.null(c_object))
    return(NA_character_)
  
  # cl_object$getCN()$getString()
  # For performance reasons:
  CCommonName_getString(CObjectInterface_getCN(c_object))
}

# get the DN of an object or list of objects as character vector
# argument `is_species` defines if the function has to check for species in the objects list
# species need to have their objectdisplayname gathered via a different function than other objects
# type can have values TRUE, FALSE, NA
# TRUE is all species, FALSE is no species, NA forces to check for each member
get_key <- function(objects, is_species = FALSE) {
  if (!is.list(objects))
    cl_objects <- list(objects)
  else
    cl_objects <- objects

  # Some APIs return generic CObjectInterface pointers that are unsafe to query
  # directly via getObjectType/getObjectDisplayName in recent bindings.
  cl_objects <-
    cl_objects %>%
    map(function(c_obj) {
      if (!inherits(c_obj, "_p_CObjectInterface"))
        return(c_obj)

      c_data_obj <- try(c_obj$getDataObject(), silent = TRUE)
      if (inherits(c_data_obj, "try-error") || is.null(c_data_obj))
        return(c_obj)

      auto_cast(c_data_obj)
    })

  is_chr <- map_lgl(cl_objects, ~ is.character(.x) && length(.x) == 1L)
  is_obj <- !is_chr

  are_species <- rep_along(cl_objects, FALSE)
  if (any(is_obj)) {
    are_species[is_obj] <- rep_along(cl_objects[is_obj], is_species)

    if (is.na(is_species)) {
      are_species[is_obj] <- map_swig_chr(cl_objects[is_obj], "getObjectType") == "Metabolite"
    }
  }

  dns <- character(length(cl_objects))

  if (any(is_chr)) {
    dns[is_chr] <- unlist(cl_objects[is_chr], use.names = FALSE)
  }

  non_species_obj <- is_obj & !are_species
  if (any(non_species_obj)) {
    dns_non_species <- map_swig_chr(cl_objects[non_species_obj], "getObjectDisplayName")
    is_reference <- map_swig_chr(cl_objects[non_species_obj], "getObjectType") == "Reference"
    dns_non_species[is_reference] <- stringr::str_replace(dns_non_species[is_reference], "^\\[(.*)\\]$", "\\1")
    dns_non_species[is_reference] <- stringr::str_replace(
      dns_non_species[is_reference],
      "\\.(ParticleNumber|Concentration|Value|Volume)$",
      ""
    )
    dns[non_species_obj] <- dns_non_species
  }

  species_obj <- is_obj & are_species
  if (any(species_obj)) {
    dns[species_obj] <-
    cl_objects[species_obj] %>%
    map(as, "_p_CMetab") %>%
    map_chr(CMetabNameInterface_createUniqueDisplayName, FALSE)
  }

  dns
}

# If a DN is a reference, wrap it in {} and escape it
escape_ref <- function(x) {
  paste0(
    "{",
    stringr::str_replace_all(x, coll("}"), "\\}"),
    "}"
  )
}

# revert escape_ref
unescape_ref <- function(x) {
  x %>%
    stringr::str_sub(2L, -2L) %>%
    stringr::str_replace_all(coll("\\}"), "}")
}

# Takes a reference object and returns either the reference "{DN}"
# or if that won't resolve back returns "<CN>"
as_ref <- function(cl_objects, c_datamodel) {
  refs <- get_key(cl_objects, is_species = FALSE)
  
  unresolvable <-
    refs %>%
    map(dn_to_object, c_datamodel = c_datamodel) %>%
    map_lgl(is.null)
  
  refs[unresolvable] <- paste0("<", map_chr(cl_objects[unresolvable], get_cn), ">")
  refs[!unresolvable] <- escape_ref(refs[!unresolvable])
  
  refs
}

# gather a list of objects from the internal COPASI Key identifier
cop_key_to_obj <- function(x) {
  map(x, CRootContainer_getKeyFactory()$get)
}
