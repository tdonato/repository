# Load required packages

install.packages("R.matlab")

require(plyr)
require(dplyr)
require(R.matlab)

# Load battery data

files <- list.files("C:/Data/battery/")

battery_cycle_total <- data.frame( cycle=character(1),
                                   F1=numeric(1), 
                                   F2=numeric(1),
                                   F3=numeric(1),
                                   F4=numeric(1),
                                   F5=numeric(1),
                                   F6=numeric(1),
                                   impedance_re=numeric(1),
                                   impedance_img=numeric(1),
                                   stringsAsFactors=FALSE )

# Read battery data and structure it into list format

for ( h in 1:length( files ) ) {
  
  df <- readMat( paste( "C:/Data/battery/", files[h], sep="" ) )
  
  profile_top <- list()
  profile_impedance_top <- list()
  
  month_top <- list()
  year_top <- list()
  hour_top <- list()
  day_top <- list()
  min_top <- list()
  sec_top <- list()
  
  month_impedance_top <- list()
  year_impedance_top <- list()
  hour_impedance_top <- list()
  day_impedance_top <- list()
  min_impedance_top <- list()
  sec_impedance_top <- list()
  
  time_top <- list()
  
  temperature_measured_top <- list()
  voltage_measured_top <- list()
  current_measured_top <- list()
  current_charge_top <- list()
  voltage_charge_top <- list()
  
  sense_current_top <- list()
  current_ratio_top <- list()
  battery_current_top <- list()
  battery_impedance_top <- list()
  rectified_impedance_top <- list()
  
  for ( i in 1:length( df[[1]] ) ) {
    
    df1 <- df[[1]][[i]]
    
    profile <- list()
    profile_impedance <- list()
    
    month <- list()
    year <- list()
    hour <- list()
    day <- list()
    min <- list()
    sec <- list()
    
    month_impedance <- list()
    year_impedance <- list()
    hour_impedance <- list()
    day_impedance <- list()
    min_impedance <- list()
    sec_impedance <- list()
    
    temperature_measured <- list()
    voltage_measured <- list()
    current_measured <- list()
    current_charge <- list()
    voltage_charge <- list()
    
    sense_current <- list()
    current_ratio <- list()
    battery_current <- list()
    battery_impedance <- list()
    rectified_impedance <- list()
    
    time <- list()
    
    batseq <- seq( from = 1, to = length( df1 ), by = 4 ) 
    
    for ( j in 1:( length( batseq ) ) ) {
      
      if ( df1[[batseq[j]]][1]=="impedance" ) {
        
        profile_impedance[[j]] <- rep( df1[[batseq[j]]][1], length( unlist( df1[[batseq[j]+3]][1] ) ) )
        
        month_impedance[[j]] <- rep( df1[[batseq[j]+2]][2], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( profile_impedance[[j]] ) ]
        year_impedance[[j]] <- rep( df1[[batseq[j]+2]][1], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( profile_impedance[[j]] ) ]
        hour_impedance[[j]] <- rep( df1[[batseq[j]+2]][4], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( profile_impedance[[j]] ) ]
        
        day_impedance[[j]] <- rep( df1[[batseq[j]+2]][3], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( profile_impedance[[j]] ) ]
        min_impedance[[j]] <- rep( df1[[batseq[j]+2]][5], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( profile_impedance[[j]] ) ]
        sec_impedance[[j]] <- rep( df1[[batseq[j]+2]][6], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( profile_impedance[[j]] ) ]
        
        sense_current[[j]] <- unlist( df1[[batseq[j]+3]][1] )[ 1:length( profile_impedance[[j]] ) ]
        current_ratio[[j]] <- unlist( df1[[batseq[j]+3]][3] )[ 1:length( profile_impedance[[j]] ) ]
        battery_current[[j]] <- unlist( df1[[batseq[j]+3]][2] )[ 1:length( profile_impedance[[j]] ) ]
        battery_impedance[[j]] <- unlist( df1[[batseq[j]+3]][4] )[ 1:length( profile_impedance[[j]] ) ]
        rectified_impedance[[j]] <- unlist( df1[[batseq[j]+3]][5] )[ 1:length( profile_impedance[[j]] ) ]
        
      } else {
        
        voltage_charge[[j]] <- unlist( df1[[batseq[j]+3]][5] )
        current_charge[[j]] <- unlist( df1[[batseq[j]+3]][4] )[ 1:length( voltage_charge[[j]] ) ]
        
        profile[[j]] <- rep( df1[[batseq[j]]][1], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( voltage_charge[[j]] ) ]
        
        month[[j]] <- rep( df1[[batseq[j]+2]][2], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( voltage_charge[[j]] ) ]
        year[[j]] <- rep( df1[[batseq[j]+2]][1], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( voltage_charge[[j]] ) ]
        hour[[j]] <- rep( df1[[batseq[j]+2]][4], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( voltage_charge[[j]] ) ]
        
        day[[j]] <- rep( df1[[batseq[j]+2]][3], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( voltage_charge[[j]] ) ]
        min[[j]] <- rep( df1[[batseq[j]+2]][5], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( voltage_charge[[j]] ) ]
        sec[[j]] <- rep( df1[[batseq[j]+2]][6], length( unlist( df1[[batseq[j]+3]][1] ) ) )[ 1:length( voltage_charge[[j]] ) ]
        
        temperature_measured[[j]] <- unlist( df1[[batseq[j]+3]][3] )[ 1:length( voltage_charge[[j]] ) ]
        voltage_measured[[j]] <- unlist( df1[[batseq[j]+3]][1] )[ 1:length( voltage_charge[[j]] ) ]
        current_measured[[j]] <- unlist( df1[[batseq[j]+3]][2] )[ 1:length( voltage_charge[[j]] ) ]
        
        time[[j]] <- unlist( df1[[batseq[j]+3]][6] )[ 1:length( voltage_charge[[j]] ) ]
        
      }
      
    }
    
    profile_top[[i]] <- unlist(profile)
    profile_impedance_top[[i]] <- unlist(profile_impedance)
    
    month_top[[i]] <- unlist(month)
    year_top[[i]] <- unlist(year)
    hour_top[[i]] <- unlist(hour)
    day_top[[i]] <- unlist(day)
    min_top[[i]] <- unlist(min)
    sec_top[[i]] <- unlist(sec)
    
    month_impedance_top[[i]] <- unlist(month_impedance)
    year_impedance_top[[i]] <- unlist(year_impedance)
    hour_impedance_top[[i]] <- unlist(hour_impedance)
    day_impedance_top[[i]] <- unlist(day_impedance)
    min_impedance_top[[i]] <- unlist(min_impedance)
    sec_impedance_top[[i]] <- unlist(sec_impedance)
    
    time_top[[i]] <- unlist(time)
    
    sense_current_top[[i]] <- unlist(sense_current)
    current_ratio_top[[i]] <- unlist(current_ratio)
    battery_current_top[[i]] <- unlist(battery_current)
    battery_impedance_top[[i]] <- unlist(battery_impedance)
    rectified_impedance_top[[i]] <- unlist(rectified_impedance)
    
    temperature_measured_top[[i]] <- unlist(temperature_measured)
    voltage_measured_top[[i]] <- unlist(voltage_measured)
    current_measured_top[[i]] <- unlist(current_measured)
    current_charge_top[[i]] <- unlist(current_charge)
    voltage_charge_top[[i]] <- unlist(voltage_charge)
    
  }
  
  profile_top <- unlist(profile_top)
  profile_impedance_top <- unlist(profile_impedance_top)
  
  month_top <- unlist(month_top)
  year_top <- unlist(year_top)
  hour_top <- unlist(hour_top)
  day_top <- unlist(day_top)
  min_top <- unlist(min_top)
  sec_top <- unlist(sec_top)
  
  month_impedance_top <- unlist(month_impedance_top)
  year_impedance_top <- unlist(year_impedance_top)
  hour_impedance_top <- unlist(hour_impedance_top)
  day_impedance_top <- unlist(day_impedance_top)
  min_impedance_top <- unlist(min_impedance_top)
  sec_impedance_top <- unlist(sec_impedance_top)
  
  sense_current_top <- unlist(sense_current_top)
  current_ratio_top <- unlist(current_ratio_top)
  battery_current_top <- unlist(battery_current_top)
  battery_impedance_top <- unlist(battery_impedance_top)
  rectified_impedance_top <- unlist(rectified_impedance_top)
  
  time_top <- unlist(time_top)
  
  temperature_measured_top <- unlist(temperature_measured_top)
  voltage_measured_top <- unlist(voltage_measured_top)
  current_measured_top <- unlist(current_measured_top)
  current_charge_top <- unlist(current_charge_top)
  voltage_charge_top <- unlist(voltage_charge_top)
  
  battery <- cbind( profile_top, year_top, month_top, day_top, hour_top, min_top, sec_top, temperature_measured_top,
                    voltage_measured_top, current_measured_top, current_charge_top, voltage_charge_top, time_top )
  
  battery <- data.frame( battery )
  names( battery )[1] <- "profile"
  names( battery )[2] <- "year"
  names( battery )[3] <- "month"
  names( battery )[4] <- "day"
  names( battery )[5] <- "hour"
  names( battery )[6] <- "min"
  names( battery )[7] <- "sec"
  names( battery )[8] <- "temperature_measured"
  names( battery )[9] <- "voltage_measured"
  names( battery )[10] <- "current_measured"
  names( battery )[11] <- "current_charge"
  names( battery )[12] <- "voltage_charge"
  names( battery )[13] <- "time"
  
  coulomb <- cbind( profile_impedance_top, year_impedance_top, month_impedance_top, day_impedance_top, hour_impedance_top, 
                    min_impedance_top, sec_impedance_top, sense_current_top, current_ratio_top, battery_current_top,
                    battery_impedance_top, rectified_impedance_top )
  
  coulomb <- data.frame( coulomb )
  names( coulomb )[1] <- "profile"
  names( coulomb )[2] <- "year"
  names( coulomb )[3] <- "month"
  names( coulomb )[4] <- "day"
  names( coulomb )[5] <- "hour"
  names( coulomb )[6] <- "min"
  names( coulomb )[7] <- "sec"
  names( coulomb )[8] <- "sense_current"
  names( coulomb )[9] <- "current_ratio"
  names( coulomb )[10] <- "battery_current"
  names( coulomb )[11] <- "battery_impedance"
  names( coulomb )[12] <- "rectified_impedance"
  
  battery[] <- lapply( battery, as.character )
  coulomb[] <- lapply( coulomb, as.character )
  
  # Calculate SOC with measurement data by Counting Coulomb method
  coulomb$cycle <- paste( coulomb$year, coulomb$month, coulomb$day, sep="_" )
  
  rectified_impedance_mean_re <- vector( mode="numeric", length=length( unique( coulomb$cycle ) ) )
  rectified_impedance_mean_img <- vector( mode="numeric", length=length( unique( coulomb$cycle ) ) )
  
  for ( i in 1:length( unique( coulomb$cycle ) ) ) {
    
    coulomb_temp <- subset( coulomb, cycle==unique( coulomb$cycle )[i] )
    rectified_impedance_mean_re[i] <- mean( Re( as.complex( as.character( coulomb_temp$rectified_impedance ) ) ), na.rm = TRUE )
    rectified_impedance_mean_img[i] <- mean( Im( as.complex( as.character( coulomb_temp$rectified_impedance ) ) ), na.rm = TRUE )
    
  }
  
  coulomb_vec <- cbind( unique( coulomb$cycle ), rectified_impedance_mean_re, rectified_impedance_mean_img )
  
  coulomb_vec <- data.frame( coulomb_vec )
  names( coulomb_vec )[1] <- "cycle"
  names( coulomb_vec )[2] <- "impedance_re"
  names( coulomb_vec )[3] <- "impedance_img"
  
  # Features extraction
  battery$cycle <- paste( battery$year, battery$month, battery$day, sep="_" )
  
  F1 <- vector( mode="numeric", length=length( unique( battery$cycle ) ) )
  F2 <- vector( mode="numeric", length=length( unique( battery$cycle ) ) )
  F3 <- vector( mode="numeric", length=length( unique( battery$cycle ) ) )
  F4 <- vector( mode="numeric", length=length( unique( battery$cycle ) ) )
  F5 <- vector( mode="numeric", length=length( unique( battery$cycle ) ) )
  F6 <- vector( mode="numeric", length=length( unique( battery$cycle ) ) )
  
  for ( i in 1:length( unique( battery$cycle ) ) ) {
    
    battery_temp <- subset( battery, cycle==unique( battery$cycle )[i] )
    battery_charge_temp <- subset( battery_temp, profile=="charge" )
    
    if ( nrow( battery_charge_temp )!=0 ) {
      
      battery_charge_temp[] <- lapply( battery_charge_temp, as.numeric )
      battery_charge_temp <- arrange( battery_charge_temp, time )
      
      battery_F1 <- subset( battery_charge_temp, voltage_measured >= 3.8 )
      battery_F1 <- subset( battery_F1, voltage_measured <= 4.2 )
      
      if ( nrow( battery_F1 )!=0 ) {
        
        F1[i] <- battery_F1$time[ nrow( battery_F1 ) ] - battery_F1$time[1]
        F4[i] <- mean( battery_F1$temperature_measured )
        
      } else {
        
        F1[i] <- 0
        F4[i] <- 0
        
      }
      
      battery_F2 <- subset( battery_charge_temp, current_measured <= 1.5 )
      battery_F2 <- subset( battery_F2, current_measured >= 0.02 )
      
      if ( nrow( battery_F2 )!=0 ) {
        
        F2[i] <- battery_F2$time[ nrow( battery_F2 ) ] - battery_F2$time[1]
        
      } else {
        
        F2[i] <- 0
        
      }
      
    } else {
      
      F1[i] <- 0
      F2[i] <- 0
      F4[i] <- 0
      
    }
    
    battery_discharge_temp <- subset( battery_temp, profile=="discharge" )
    
    if ( nrow( battery_discharge_temp )!=0 ) {
      
      battery_discharge_temp[] <- lapply( battery_discharge_temp, as.numeric )
      battery_discharge_temp <- arrange( battery_discharge_temp, time )
      
      battery_F3 <- subset( battery_discharge_temp, voltage_measured >= 3.8 )
      battery_F3 <- subset( battery_F3, voltage_measured >= 3.0 )
      
      if ( nrow( battery_F3 )!=0 ) {
        
        F3[i] <- battery_F3$time[ nrow( battery_F3 ) ] - battery_F3$time[1]
        F5[i] <- mean( battery_F3$temperature_measured )
        
      } else {
        
        F3[i] <- 0
        F5[i] <- 0
        
      }
      
      battery_cutoff_voltage <- arrange( battery_discharge_temp, voltage_measured )
      
      F6[i] <- battery_cutoff_voltage$voltage_measured[1]
      
    } else {
      
      F3[i] <- 0
      F5[i] <- 0
      F6[i] <- 0
      
    }
    
  }
  
  battery_cycle <- cbind( unique( battery$cycle ), F1, F2, F3, F4, F5, F6 )
  
  battery_cycle <- data.frame( battery_cycle )
  names( battery_cycle )[1] <- "cycle"
  names( battery_cycle )[2] <- "F1"
  names( battery_cycle )[3] <- "F2"
  names( battery_cycle )[4] <- "F3"
  names( battery_cycle )[5] <- "F4"
  names( battery_cycle )[6] <- "F5"
  names( battery_cycle )[7] <- "F6"
  
  # Obtain batery cycle's impedance 
  battery_cycle$impedance_re <- 0
  battery_cycle$impedance_img <- 0
  
  match_vec <- match( as.character( battery_cycle$cycle ), as.character( coulomb_vec$cycle ) )
  
  nullIdx <- as.array( which( is.na( match_vec ) ) )
  match_vec[ nullIdx ] <- match_vec[ nullIdx-1 ]  
  
  battery_cycle$impedance_re <- coulomb_vec$impedance_re[ match_vec ]
  battery_cycle$impedance_img <- coulomb_vec$impedance_img[ match_vec ]
  
  battery_cycle_total <- rbind( battery_cycle_total, battery_cycle )
  
}

# Obtain historical data

battery_cycle_total <- battery_cycle_total[ 2:nrow( battery_cycle_total ), ]