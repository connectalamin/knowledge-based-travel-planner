# Voyager - Knowledge-Based Travel Planner

A modern, AI-powered travel recommendation system built with SWI-Prolog that uses knowledge-based reasoning to suggest perfect destinations based on your preferences. Features a beautiful, responsive web interface with real-time recommendations.

## 🚀 Features

- **AI-Powered Recommendations**: Intelligent matching algorithm that scores destinations based on multiple criteria
- **Modern UI**: Beautiful, responsive interface with card-based results display
- **Real-Time Scoring**: See why each destination matches your preferences with detailed scoring breakdown
- **Knowledge-Based Reasoning**: Powered by SWI-Prolog for intelligent decision-making
- **Multiple Criteria Matching**: 
  - Budget level (Low/Medium/High)
  - Climate preferences (Warm/Cool/Cold)
  - Visa requirements
  - Safety ratings
  - Seasonal timing
  - Activity interests
  - Food preferences
  - Continent/Region

## 📋 Prerequisites

- SWI-Prolog installed on your system
- A modern web browser

## 🛠️ Setup

1. Navigate to the project directory in SWI-Prolog:
```prolog
cd('F:/Web/knowledge-based-travel-planner').
```

2. Start the SWI-Prolog shell:
```bash
swipl
```

3. Within the SWI-Prolog shell, load and start the server:
```prolog
?- consult('travel_server.pl').
?- server(8000).
```

You should see:
```
% Started server at http://localhost:8000/
```

4. Open your browser and navigate to:
```
http://localhost:8000/
```

## 💡 Example Usage

### Example 1: Budget-Friendly Asian Adventure
- **Budget:** Low Budget ($)
- **Days Available:** 3
- **Preferred Climate:** Warm
- **Visa Status:** Visa Free
- **Continent:** Asia
- **Travel Month:** 2 (February)
- **Desired Activities:** street_food, culture
- **Minimum Safety Level:** 3
- **Food Preference:** Street Food

**Expected Result:** **Hanoi** - Perfect match for budget travelers seeking authentic street food and culture in warm weather.

### Example 2: Cool Climate Hiking Trip
- **Budget:** Medium Budget ($$)
- **Days Available:** 7
- **Preferred Climate:** Cool
- **Visa Status:** Visa on Arrival
- **Continent:** Asia
- **Travel Month:** 10 (October)
- **Desired Activities:** hiking, culture
- **Minimum Safety Level:** 3
- **Food Preference:** Any

**Expected Result:** **Kathmandu** - Ideal for hiking enthusiasts with cool climate and visa on arrival.

### Example 3: Luxury European Experience
- **Budget:** High Budget ($$$)
- **Days Available:** 5
- **Preferred Climate:** Cold
- **Visa Status:** Schengen Required
- **Continent:** Europe
- **Travel Month:** 5 (May)
- **Desired Activities:** museum, romance, food
- **Minimum Safety Level:** 4
- **Food Preference:** Fine Dining

**Expected Result:** **Paris** - Perfect for a romantic luxury getaway with museums and fine dining.

## 🏗️ Project Structure

```
knowledge-based-travel-planner/
├── travel_server.pl      # Prolog server with knowledge base and matching logic
├── www/
│   ├── index.html        # Main HTML interface
│   ├── style.css         # Modern styling and responsive design
│   └── app.js            # Frontend JavaScript for API communication
└── Readme.md             # This file
```

## 🎨 UI Features

- **Sidebar Filters**: Easy-to-use form with icons for all preference categories
- **Card-Based Results**: Beautiful destination cards with:
  - Match percentage scores
  - Visual match indicators
  - Destination images
  - Detailed scoring breakdown
- **Responsive Design**: Works seamlessly on desktop and mobile devices
- **No Page Scrolling**: Everything fits in one screen view for optimal UX
- **Loading States**: Smooth animations during recommendation processing

## 🔧 How It Works

1. **User Input**: Fill out the preference form in the sidebar
2. **API Request**: Frontend sends JSON request to `/recommend` endpoint
3. **Knowledge Processing**: Prolog server:
   - Parses the request
   - Matches against destination knowledge base
   - Scores each destination based on criteria
   - Sorts by relevance
4. **Results Display**: Frontend renders beautiful cards with match details

## 📊 Scoring System

Each destination is scored based on:
- **Budget Match** (0-1 points)
- **Climate Match** (0-1 points)
- **Visa Match** (0-1 points)
- **Continent Match** (0-1 points)
- **Seasonal Match** (0-1 points)
- **Safety Match** (0-1 points)
- **Food Match** (0-1 points)
- **Activity Match** (0-1 points, averaged for multiple activities)

Total score ranges from 0-8, with higher scores indicating better matches.

## 🌍 Available Destinations

The system includes destinations such as:
- Tokyo, Osaka, Kyoto (Japan)
- Bangkok (Thailand)
- Kathmandu (Nepal)
- Istanbul (Turkey)
- Paris (France)
- Dubai (UAE)
- Colombo (Sri Lanka)
- Hanoi (Vietnam)

*More destinations can be easily added to the knowledge base in `travel_server.pl`*

## 🐛 Troubleshooting

**Server won't start:**
- Make sure port 8000 is not in use
- Check that `travel_server.pl` is in the correct directory
- Verify SWI-Prolog is properly installed

**No results returned:**
- Try adjusting your criteria (especially safety level and visa status)
- Check that your selected continent has matching destinations
- Ensure activities match destination keywords

**CSS/JS not loading:**
- Verify the server is running on port 8000
- Check browser console for 404 errors
- Ensure files are in the `www/` directory

## 📝 License

This project is open source and available for educational purposes.

## 🤝 Contributing

Feel free to add more destinations to the knowledge base or improve the matching algorithm!
