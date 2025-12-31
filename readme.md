# Knowledge-Based Travel Planner

A Prolog-based expert system that provides intelligent travel destination recommendations based on user preferences.

## 🚀 How to Start

### Step 1: Install SWI-Prolog
Download and install from [swi-prolog.org](https://www.swi-prolog.org/Download.html)

### Step 2: Start the Server

1. **Open SWI-Prolog** (search "SWI-Prolog" in Windows Start menu)

2. **Load the server file:**
   - Click **File** → **Consult...**
   - Navigate to this project folder
   - Select **`server.pl`**
   - Click **Open**

3. **Start the server:**
   - In the Prolog prompt, type:
   ```prolog
   ?- server.
   ```
   - Press **Enter**

4. **You'll see:**
   ```
   ==========================================
   ✅ Server started successfully!
   ==========================================
   Server running on: http://localhost:8000

   📍 API Endpoints:
      POST /recommend - Get travel recommendations
      GET  /activities - List all activities

   🎯 Frontend UI: http://localhost:8000/app
   ==========================================
   ```

### Step 3: Use the App

Open your browser and go to: **http://localhost:8000/app**

That's it! 🎉 Set your travel preferences and get personalized destination recommendations.

## 🛑 How to Stop

- Close the SWI-Prolog window, OR
- Type `?- halt.` in the Prolog prompt

## 🏗️ Project Structure

```
knowledge-based-travel-planner/
├── server.pl       # HTTP server + routes
├── rules.pl        # Recommendation logic
├── travel_db.pl    # Destination database
├── index.html      # Frontend UI
├── script.js       # Frontend JavaScript
└── style.css       # Frontend styling
```
