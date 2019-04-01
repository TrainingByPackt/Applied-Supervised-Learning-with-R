import os, io, boto3, json, csv

# grab environment variables
ENDPOINT_NAME = os.environ['ENDPOINT_NAME']
runtime= boto3.client('runtime.sagemaker')

def lambda_handler(event, context):
    print("Received event: " + json.dumps(event, indent=2))

    data = json.loads(json.dumps(event))
    payload = data['data']
    print(payload)

    response = runtime.invoke_endpoint(EndpointName=ENDPOINT_NAME,
                                       ContentType='text/csv',
                                       Body=bytes(str(payload),'utf'))

    result = json.loads(response['Body'].read().decode())
    predicted_label = 'yes' if result >0.5 else 'no'
    return predicted_label
