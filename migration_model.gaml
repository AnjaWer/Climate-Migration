/**
* Name: Agent-based migration model 
* Author: anja
*/

model MigrationModel

// Definition of global variables and actions
global {
	// reading in shapefile containing the regions and region-level data (e.g. population)
	// add file_path
	file shape_file_regions <- file("file_path"); 
	// reporting total migrant stock and flow 
	int migrant_stock <- migrant_stock update: household count (each.migrated); 
	int migrant_flow;
	int hh_per_agent <- 20000; 	   		// currently: one agent represents 20000 households
	int ave_hh_size <- 5;				// average household size 	
	int run_time <-20;             		        // e.g. in years
	float pop_growth <- 0.0;                        // population growth rate
	int switch_slow_onset <- 0;    			// binary (1 = linear decrease of agricultural output)
	float p_shock <- 0.0;          			// probability of climate shock per year
	int shock <- 0;					// binary (1 = shock in given year)
	int t_shock <- 0;				// storing most recent year of shock 
	float financial_thresh <- 50.0;		 	// minimum savings to be able to migrate 
	float adaptation_thresh <- 0.5;		 	// currently: threshold of household vulnerability 
	float zero_social <- 0.05;			// percentage of households with social threshold = 0
	list<region> net_migration_data <- [];
	
	// creating region agents, assigning regional level data, creating household agents
	init {
		create region from: shape_file_regions with: [name::read ("Name"), region_population::read("Population")];
		ask region of_species region {
			int n <- round(self.region_population/(ave_hh_size*hh_per_agent));
			create household number:n {
				location <- myself.location;	
				origin <- myself.name;
				region_name <- myself.name;
			}		
		}
		// assinging minority of households who are not influenced by social factors in their migration decision
		ask household of_species household {
			if (rnd(0.0,1.0) < zero_social) {
				social_thresh <- 0.0;
			}
		}
	}
	
	geometry shape <- envelope(shape_file_regions); 
	
	reflex reset_migrant_flow {
		migrant_flow <- 0;
		}
	
	reflex reset_shock when: cycle>t_shock+1 {
		shock <- 0; 
		}
		
	// climate shocks happen randomly with a fixed probability 
	reflex climate_shock {
		if (rnd(0.0,1.0) < p_shock) {
				shock <- 1;
				t_shock <- cycle;
				write "shock in year:";
				write cycle;}
		}
			
	reflex endSimulation when: cycle>=run_time {
		do pause;
	}
}

// Definition of representative household agents
species household {
	int age <- rnd(15,60) update: age + 1;  		// age of household head 
	int education <- rnd_choice([0.5,0.5]); 		// education level of household head
	float income <- rnd(30.0,100.0);			// income from agriculture
//	float nonfarm_income <- rnd_choice([0.8,0.2]); 		// income from nonfarm work
//	float hh_needs <- rnd(30.0,100.0);	
	float savings <- rnd(0.0,100.0) update: savings + rnd(-0.05,0.05)*income; // household savings
	float vulnerability <- 0.0;				// initial value, updated every step (see below)
	string origin;						// region of origin
	string region_name;					// current region of residence
	bool migrated <- false; 
	float social_norm <- 0.0;				// initial social norm, updated every step (see below) 
	list<household> social_network <- [];
	float social_thresh <- rnd(0.0, 0.4);   		// distribution of social thresholds of majority of households
	float alpha_econ <- 0.0; 				// weights for decision factors for choice of destination, initialized below
	float alpha_dist <- 0.0;
	float alpha_social <- 0.0;
	
	init {	
		// initializing social network (community) as 10 agents from the same region of origin
		social_network <- sample(household where ((each.origin = origin) and (each.name != name)), 10, false);
		
		// initiatlizing weights for decision factors (choice of destination)
		let alpha1 <- rnd(0.0,1.0);
		let alpha2 <- rnd(0.0,1.0);
		let alpha3 <- rnd(0.0,1.0);
		let sum <- alpha1 + alpha2 + alpha3;
		alpha_econ <- alpha1/sum;
		alpha_dist <- alpha2/sum;
		alpha_social <- alpha3/sum;
		
		//	next steps: defining income, education, other characteristics based on region-level data
		//	ask region where (each.name = region_name) {
		//		}
	}
	
	// updating vulnerability based on cropyield (reginal level) and household dependency from agriculture
	reflex update_vulnerability {
		ask region where (each.name = region_name) {
			myself.vulnerability <- 1-r_cropyield*myself.income/100;
		}
	}
	  	
	// updating social norm, defined as percentage of migrants in one's social network
	reflex update_social_norm {
		let migrant_count <- 0;
		let total_count <- 0;
		ask social_network {
			if migrated {migrant_count <- migrant_count +1;}
			total_count <- total_count+1;
			if total_count != 0 {
			social_norm <- migrant_count/total_count;}
		}
  	}
	
	// evaluating migration thresholds, if all three are met, opt for migration, else stay
	reflex migration_decision {
		if !migrated {
			if (adaptation_thresh <= vulnerability){
				if (social_thresh <= social_norm) {
					if (financial_thresh <= savings)
					{ do migrate; }
				}
			}
		}
	}
	
	// migration, i.e. assign new location, new current region
	action migrate {
		migrant_flow <- migrant_flow+1;
	    migrated <- true;
		string destination <- choose_destination(); 
		write "1 agent from " + region_name + " moved to " + destination;
		ask one_of (region where (each.name = destination))
		{
			myself.location <- location;
	    	myself.region_name <- name;
	    	self.migrants_in <- self.migrants_in+1;
		}

	    ask one_of (region where (each.name = origin)) {
			self.migrants_out <- self.migrants_out+1;
		}
    }
    
    // choosing migration destination: region with highest pull factor (function of economic opportunities, distance, social capital)
    // economic opportunities approximated by average income of agents in region
    // social capital is the number of community members who has moved to the region 
    // distance from region centroid 
    string choose_destination {
    	list<region> other_regions <- region.population where (each.name != origin);
	 	map<region,float> econ_opps <- other_regions as_map (each::(each.ave_income));
	 	map<region,float> distance <- other_regions as_map (each::(each.location distance_to location));
	 	map<region,int> social_capital <- other_regions as_map (each::(length(each.residents where (each.origin = origin))));

	 	// normalize to max (min) value
	 	let max_econ <- max(econ_opps);
	 	let min_dist <- min(distance);
	 	let max_social <- max(social_capital);
	 
	 	map<region,float> econ_opps_norm <- other_regions as_map (each::(each.ave_income/max_econ));
	 	map<region,float> distance_norm <- other_regions as_map (each::(1/(each.location distance_to location))*min_dist);
	 	if max_social > 0 {
	 		map<region,float> social_capital <- other_regions as_map (each::(length(each.residents where (each.origin = origin)))/max_social);
	 	}
	 	
	 	// calculating pull factors for each regions based on weighted decision factors
	 	map<region,float> destination_map <- other_regions as_map (each::(alpha_econ*econ_opps_norm[each] + alpha_dist*distance_norm[each] + alpha_social*social_capital[each]));
	  	// choose destination with highest pull factor
	 	region destination <- destination_map index_of max(destination_map);
	 	return destination.name;
    }
  
  	// optional: display household agents
	aspect default {
	draw circle(1.0) color: #blue;
	}
}

// Definition of region agents
species region {
	int region_population;				// population defined based on data input (see shapefile)
	int net_migration <- 0;				// reporting net migration
	int migrants_in <- 0;
	int migrants_out <- 0;
	float ave_income <- 100.0;			// proxy for economic opportunities, dynamic, updated every step (see below)
	float r_cropyield <- 1.0;			// agricultural output (as fraction of initial output)
	int exposure <- rnd(0,1);			// regional exposure level to climate events 
	list<household> residents <- agents_inside(self); // list of household agents living in region
	rgb region_color <- #gray;
	init {
	}
	
	// updating regional level net migration, list of residents, economic opportunities
	reflex update_data {
		net_migration <- (migrants_in - migrants_out);
		residents <- agents_inside(self);
		let total_income <- 0.0;
		ask residents {
			total_income <- total_income+self.income;
		}
		if length(residents)=0 {
			ave_income <- 0.0;
		}
		else{ave_income <- total_income/length(residents);}
	}

	// updating the regional agricultural output
	// currently: slow onset events lead to linear decrease, e.g. -2% per year
	// currently: shocks lead to -70% decrease for two consecutive years
	reflex update_r_cropyield {
		r_cropyield <- r_cropyield - exposure*switch_slow_onset*0.02;
		r_cropyield <- r_cropyield - exposure*shock*0.7;
	}
		
	// displaying regions and colors according to net migration
	reflex update_color {
		if (net_migration > -5) and (net_migration <= 5) {
			region_color <- #gray;
			}
		else if (net_migration <= -5) {
			region_color <- rgb(0, 0, 139);
		}
		else if (net_migration > 5) and (net_migration <= 10) {
			region_color <- #orange;
		}
		else if net_migration > 10 {
			region_color <- #red;
		}
		else {
			region_color <- #gray;
		}
	}
	aspect default {
	draw shape color: region_color border: #black;
	}
	
}

// Definition of the user interface 
experiment main_experiment type: gui {
	parameter "Run time [years]" var: run_time;
	parameter "Slow onset" var: switch_slow_onset min: 0 max: 1 category: "Climate" ;
	parameter "Shocks [prob. per year]" var: p_shock min: 0.0 max: 0.1 category: "Climate" ;
	
	parameter "Population growth" var: pop_growth min: 0.0 max: 1.0 category: "Demographic" ;

//    parameter "x:" var: shock category: "Socioeconomic data" ;
    parameter "Shapefile:" var: shape_file_regions category: "GIS data" ;
	
	output {
	layout horizontal([0::5000,vertical([1::5000,2::5000])::5000]) tabs:true toolbars:true;

 	display map {
 		species region;       
    }
    
    display chart_display1 {
    	chart "Total migrant stock" type: series x_label:"time[years]" y_label: "Number of household agents" {
    		data "Migrant stock" value: migrant_stock;
    	}
    }
    display chart_display2 {
    	chart "Total migrant flow" type: series x_label:"time[years]" y_label: "Number of household agents" {
    		data "Migrant flow" value: migrant_flow;
    	}
    }
}
//    reflex save_data {
//    	save  [cycle, migrant_stock, migrant_flow]  to: "model_data.csv" type: "csv" rewrite: false;
//    	save region to: "region_data.csv" type: "csv" rewrite: false;
//    }
}





