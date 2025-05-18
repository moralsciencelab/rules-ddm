/// Rules stimuli /
/*shoes*/
var startpage_shoes = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: `<b>To keep her house clean, Mary announced: "No one may wear shoes in the house".</b><br>
               <br>
               On the following pages, we will show you several examples of behaviours that may or may not violate the rule.<br>
               <br>
               Each action will be presented on a separate page.<br>
               <br>
               Press the assigned keys to indicate whether or not the person violated the rule.<br>
               <br>
               Remember to answer as quickly as possible.<br>
               <br>
               Press any key to begin.`,
};

/*cars*/
var startpage_cars = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: `<b>To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park".</b><br>
               <br>
               On the following pages, we will show you several examples of behaviours that may or may not violate the rule.<br>
               <br>
               Each action will be presented on a separate page.<br>
               <br>
               Press the assigned keys to indicate whether or not the person violated the rule.<br>
               <br>
               Remember to answer as quickly as possible.<br>
               <br>
               Press any key to begin.`,
};

/*trainstation*/
var startpage_trainstation = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: `<b>To avoid people spending the night at the train station, the manager posted a rule: "No sleeping allowed on the benches".</b><br>
               <br>
               On the following pages, we will show you several examples of behaviours that may or may not violate the rule.<br>
               <br>
               Each action will be presented on a separate page.<br>
               <br>
               Press the assigned keys to indicate whether or not the person violated the rule.<br>
               <br>
               Remember to answer as quickly as possible.<br>
               <br>
               Press any key to begin.`,
};

/*phones*/
var startpage_phones = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: `<b>To get students to pay attention in class, the headmaster announced: "Phones may not be used in the classroom".</b><br>
               <br>
               On the following pages, we will show you several examples of behaviours that may or may not violate the rule.<br>
               <br>
               Each action will be presented on a separate page.<br>
               <br>
               Press the assigned keys to indicate whether or not the person violated the rule.<br>
               <br>
               Remember to answer as quickly as possible.<br>
               <br>
               Press any key to begin.`,
};

/*deer*/
var startpage_deer = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: `<b>To protect the deer population, parliament enacts a law: "It is an offense to shoot deer".</b><br>
               <br>
               On the following pages, we will show you several examples of behaviours that may or may not violate the rule.<br>
               <br>
               Each action will be presented on a separate page.<br>
               <br>
               Press the assigned keys to indicate whether or not the person violated the rule.<br>
               <br>
               Remember to answer as quickly as possible.<br>
               <br>
               Press any key to begin.`,
};

/*lab*/
var startpage_lab = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: `<b>To avoid accidents, the director of a scientific laboratory issued a rule: "Access for laboratory personnel only".</b><br>
               <br>
               On the following pages, we will show you several examples of behaviours that may or may not violate the rule.<br>
               <br>
               Each action will be presented on a separate page.<br>
               <br>
               Press the assigned keys to indicate whether or not the person violated the rule.<br>
               <br>
               Remember to answer as quickly as possible.<br>
               <br>
               Press any key to begin.`,
};

/*alcohol*/
var startpage_alcohol = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: `<b>To reduce traffic accidents, a zero tolerance law is passed: "Driving with any trace of alcohol is strictly forbidden".</b><br>
               <br>
               On the following pages, we will show you several examples of behaviours that may or may not violate the rule.<br>
               <br>
               Each action will be presented on a separate page.<br>
               <br>
               Press the assigned keys to indicate whether or not the person violated the rule.<br>
               <br>
               Remember to answer as quickly as possible.<br>
               <br>
               Press any key to begin.`,
};

/*restaurant*/
var startpage_restaurant = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: `<b>A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                            To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant".</b><br>
               <br>
               On the following pages, we will show you several examples of behaviours that may or may not violate the rule.<br>
               <br>
               Each action will be presented on a separate page.<br>
               <br>
               Press the assigned keys to indicate whether or not the person violated the rule.<br>
               <br>
               Remember to answer as quickly as possible.<br>
               <br>
               Press any key to begin.`,
};

/* define test stimuli for timeline variable */

/*shoes*/
var test_stimuli_shoes = [
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A guest wears stilettos and the carpets get pretty dirty.</b><br>`,
  },
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A guest wears wet deck shoes and dirties the floors.</b><br>`,
  },
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A guest wears muddy sneakers and dirties the carpets.</b><br>`,
  },
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A guest wears brand new shoes and keeps the floor clean.</b><br>`,
  },
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A guest wears clean stilettos and keeps the carpet clean.</b><br>`,
  },
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A guest wears his new sneakers and the floor stays clean.</b><br>`,
  },
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A guest walks around barefoot and dirties the carpets.</b><br>`,
  },
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A guest walks in with muddy socks and dirties the carpet.</b><br>`,
  },
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A barefoot guest has a bleeding toe and stains the carpets.</b><br>`,
  },
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A guest walks around barefoot and keeps the carpet clean.</b><br>`,
  },
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A guest walks around in socks and keeps the floor clean.</b><br>`,
  },
  {
    stimulus: `To keep her house clean, Mary announced: "No one may wear shoes in the house"<br><br><br><b>A guest walks around barefoot and the floor stays clean.</b><br>`,
  },
];

/*cars*/
var test_stimuli_cars = [
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>An artist installs a race car in the park to honour local heroes.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>A landscaper parks a truck in the park to remove a tree.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>A man drives a tow-truck slowly into the park to remove a car.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>A person speeds through the park on their electric scooter.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>A motorcycle courier takes a shortcut through the park.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>A group of friends cycle through the park on a beer bike.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>A person jogs through the park to get some daily exercise.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>A couple walk their new baby through the park in a stroller.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>A landscaper pushes a grey wheelbarrow through the park.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>A person test-drives their new car through the park at noon.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>A taxi driver takes a shortcut through the park to save time.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the local authority issued a rule: "No cars are allowed inside the park""<br><br><br><b>A red party limo drives through the park at the DJ's request.</b><br>`,
  },
];

/*trainstation*/
var test_stimuli_trainstation = [
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A vagrant spends the whole night on a bench but does not sleep.</b><br>`,
  },
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A vagrant spends the night sleeping on the station floor.</b><br>`,
  },
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A backpacker spends the night sleeping behind a station bench.</b><br>`,
  },
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A businesswoman enters the station and buys her train ticket.</b><br>`,
  },
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A commuter sits on a station bench while waiting for her train.</b><br>`,
  },
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A backpacker looks for her lost phone on the station platform.</b><br>`,
  },
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A vagrant spends the night sleeping on a bench at the station.</b><br>`,
  },
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A backpacker spends the night sleeping on a station bench.</b><br>`,
  },
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A nightclub dancer falls asleep on a station bench after work.</b><br>`,
  },
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A commuter briefly falls asleep on a bench waiting for his train.</b><br>`,
  },
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A backpacker sits on a station bench and takes a five-minute nap.</b><br>`,
  },
  {
    stimulus: `To avoid people spending the night at the train station, the manager posted a rule:<br> "No sleeping allowed on the benches"<br><br><br><b>A shift worker briefly dozes on a bench waiting for her manager.</b><br>`,
  },
];

/*phones*/
var test_stimuli_phones = [
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A student consults the textbook to answer a question in class.</b><br>`,
  },
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A student takes notes by hand to help her understand a lesson.</b><br>`,
  },
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A student arrives early to class and takes out her pencil case.</b><br>`,
  },
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A student texts friends on her phone and pays no attention.</b><br>`,
  },
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A student plays a game on his phone instead of paying attention.</b><br>`,
  },
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A student browses her phone instead of following her lesson.</b><br>`,
  },
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A student uses his phone's calculator for the in-class assignment.</b><br>`,
  },
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A diabetic student gets an alert on her phone to take her insulin.</b><br>`,
  },
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A student uses her phone as a paperweight to keep papers together.</b><br>`,
  },
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A student plays on his tablet instead of paying attention in class.</b><br>`,
  },
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A student messages friends on his laptop and pays no attention.</b><br>`,
  },
  {
    stimulus: `To get students to pay attention in class, the headmaster announced:<br> "Phones may not be used in the classroom"<br><br><br><b>A student reads news on her tablet and doesn't follow the lesson.</b><br>`,
  },
];

/*deer*/
var test_stimuli_deer = [
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A farmer sprays poison in his woodland to kill the local deer.</b><br>`,
  },
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A man catches and kills a lone baby deer with a hunting knife.</b><br>`,
  },
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A man catches a deer using a trap and gives it a lethal injection.</b><br>`,
  },
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A man heads deep into the local forest and sees several deer.</b><br>`,
  },
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A woman hikes in the woods carrying a flashlight and a lunch box.</b><br>`,
  },
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A man takes a walk in the park and photographs the deer there.</b><br>`,
  },
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A man heads deep into the local forest and shoots a deer.</b><br>`,
  },
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A woman enters a wildlife reserve where she shoots two deer.</b><br>`,
  },
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A hunter, armed with a rifle, fires on the first deer he sees.</b><br>`,
  },
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A farmer shoots a deer to end its suffering after it was run over.</b><br>`,
  },
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A hiker finds a dying deer and shoots it to relieve its pain.</b><br>`,
  },
  {
    stimulus: `To protect the deer population, parliament enacts a law: "It is an offense to shoot deer"<br><br><br><b>A vet shoots a deer with a disease that threatens the whole herd.</b><br>`,
  },
];

/*lab*/
var test_stimuli_lab = [
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>A scientist working at the laboratory starts a new experiment.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>A member of the public walks past the laboratory while going home.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>A tourist walks past the laboratory on his way to the wax museum.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>An amateur scientist enters the laboratory to take photographs.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>A thief enters the laboratory overnight to find and steal equipment.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>A student enters the laboratory to observe the scientists' work.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>A skilled technician enters the lab to discuss a job opening.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>A recently retired scientist returns to collect his briefcase.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>A retired scientist visits the lab to offer advice to new hirees.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>A lab member stays after work and gets drunk in the laboratory.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>A lab scientist performs a risky task while feeling dizzy and tired.</b><br>`,
  },
  {
    stimulus: `To avoid accidents, the director of a scientific laboratory issued a rule:<br> "Access for laboratory personnel only"<br><br><br><b>A lab member spends the night sleeping on fragile lab equipment.</b><br>`,
  },
];

/*alcohol*/
var test_stimuli_alcohol = [
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>A sober man rides his motorcycle home after playing board games.</b><br>`,
  },
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>A woman drives home after drinking soda at the movie theater.</b><br>`,
  },
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>A sober man re-parks his car in the cul-de-sac to let a guest out.</b><br>`,
  },
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>A young man drives home after drinking heavily at a music festival.</b><br>`,
  },
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>One night two drunk motorbikers race each other on a dark backroad.</b><br>`,
  },
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>A woman drives home after drinking whiskey at the movie theater.</b><br>`,
  },
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>A neighbor sips on wine and re-parks his car in the cul-de-sac.</b><br>`,
  },
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>A person drives to work after using alcohol-based mouthwash.</b><br>`,
  },
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>A drunk man drives his wife to the hospital as she goes into labor.</b><br>`,
  },
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>A young man rides his motorcycle home after smoking marihuana.</b><br>`,
  },
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>One stormy night two sober motorbikers race each other on a dark backroad.</b><br>`,
  },
  {
    stimulus: `To reduce traffic accidents, a zero tolerance law is passed:<br> "Driving with any trace of alcohol is strictly forbidden"<br><br><br><b>A woman drives home after taking ecstasy at a music festival.</b><br>`,
  },
];

/*restaurant*/
var test_stimuli_restaurant = [
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>A customer brings her pet bulldog into the restaurant for lunch.</b><br>`,
  },
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>A couple bring their pet foxhound into the restaurant for dinner.</b><br>`,
  },
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>A customer walks into the restaurant with his pet greyhound.</b><br>`,
  },
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>A blind person brings his trained guide dog into the restaurant.</b><br>`,
  },
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>A woman brings her trained therapy labrador into the restaurant.</b><br>`,
  },
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>A man carries an injured dog into the restaurant to call for help.</b><br>`,
  },
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>A customer brings his pet monkey into the restaurant for lunch.</b><br>`,
  },
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>A customer brings his new pet fox into the restaurant for lunch.</b><br>`,
  },
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>A customer walks into the restaurant with his new pet parrot.</b><br>`,
  },
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>Three colleagues have a meeting over dinner at the restaurant.</b><br>`,
  },
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>A couple have breakfast together at the restaurant before work.</b><br>`,
  },
  {
    stimulus: `A  restaurant's customers had their meals interrupted by the misbehaviour of a diner's pet dog.<br>
                               To address these concerns, the owner posted a rule: "No dogs allowed in the restaurant"<br><br><br><b>A customer uses the restaurant WC while waiting for her food.</b><br>`,
  },
];
