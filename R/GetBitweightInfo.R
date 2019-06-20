GetBitweightInfo = function(SN, config, bitweight = NaN, bitweight_V = NaN, bitweight_Pa = NaN, units = 'Pa', version = NaN){

  if(is.na(version)){
    version = GetVersion(SN)
  }

  ## Tricky part: determining all the bitweights while remaining consistent with user input (and identifying inconsistencies, if present). A few possibilities: user can provide no inputs (default, preferred); user can omit 'bitweight' while specifying bitweight_X and setting consistent units; or user can provide 'bitweight' and omit bitweight_X. The config input is totally ignored if the user provides any information.
  
  if(is.na(bitweight) && !((!is.na(bitweight_V) && units == 'V') || (!is.na(bitweight_Pa) && units == 'Pa'))){ ## preferred option: user leaves everything blank and we use the defaults

    specs = GemSpecs[GemSpecs$version == version, ]
    
    if(config$adc_range == 0){ ## high gain
      multiplier = 1
      bitweight_V = specs$bitweight_V * multiplier
      bitweight_Pa = specs$bitweight_Pa * multiplier
    }else if(config$adc_range == 1){ ## low gain
      multiplier = 2
      bitweight_V = specs$bitweight_V * multiplier
      bitweight_Pa = specs$bitweight_Pa * multiplier
    }else{ ## This should never actually happen, but just in case
      units = 'counts'
      bitweight_V = NaN
      bitweight_Pa = NaN
      warning(paste('invalid config option: adc_range =', config$adc_range, '; using bitweight = 1, units = counts, and NaN bitweight_V and bitweight_Pa'))
    }
    if(units == 'V'){
      bitweight = bitweight_V
    }else if(units == 'Pa'){
      bitweight = bitweight_Pa
    }else if(units == 'counts' || units == 'Counts'){
      bitweight = 1
    }else{
      stop('Invalid units')
    }

    ## Possibility 2: User specified a 'bitweight_Pa'/'bitweight_V' and consistent units, and no 'bitweight'
  }else if(is.na(bitweight) && (!is.na(bitweight_V) && units == 'V')){ ## user left bitweight NaN but specified bitweight_V and units are V
    bitweight = bitweight_V
  }else if(is.na(bitweight) && (!is.na(bitweight_V) && units == 'V')){ ## user left bitweight NaN but specified bitweight_V and units are V
    bitweight = bitweight_Pa

    ## Possibility 3: User specified a bitweight. Check to be sure it's not inconsistent with other input.
  }else if(!is.na(bitweight)){ ## at this point, the user specified a bitweight. make sure it's consistent with bitweight_Pa, bitweight_V, and the units
    if(units == 'V'){
      if(is.na(bitweight_V)){
        bitweight_V = bitweight
      }else if(bitweight_V != bitweight){
        stop('Inconsistent bitweights and units')
      }
    }else if(units == 'Pa'){
      if(is.na(bitweight_Pa)){
        bitweight_Pa = bitweight
      }else if(bitweight_Pa != bitweight){
        stop('Inconsistent bitweights and units')
      }
    }else if(units == 'Counts' || units == 'counts'){
      if(bitweight != 1){
        stop('Inconsistent bitweights and units')
      } 
    }else{ ## do nothing if units are something else and bitweight is provided
    }

    ## last possibility: bitweight is not provided, and cannot be determined from other inputs
  }else{
    stop('Invalid bitweights and units')
  }

  return(list(
    bitweight = bitweight,
    units = units,
    bitweight_V = bitweight_V,
    bitweight_Pa = bitweight_Pa,
    version = version
  ))
}
GetVersion = function(SN){
  #data(GemSpecs)
  SN = as.numeric(SN)
  return(GemSpecs$version[SN >= GemSpecs$min_SN & SN <= GemSpecs$max_SN])
}
