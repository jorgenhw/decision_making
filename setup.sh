# install requirements.txt for project
# Usage: source setup.sh

# create virtual environment
python3 -m venv venv_Decision_making

# activate virtual environment
source venv_Decision_making/bin/activate

# Install requirements
python3 -m pip install --upgrade pip

pip3 install -r requirements.txt

# INFO message
echo "Successfully installed requirements.txt"