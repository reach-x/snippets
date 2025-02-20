from aws_cdk import core


class MyStack(core.Stack):

    def __init__(self, scope: core.Construct, id: str, **kwargs) -> None:
        super().__init__(scope, id, **kwargs)

        # Get the available EC2 instance types
        response = ec2.describe_instance_types(MaxResults=100)

        instance_types = []
        for instance_type in response["InstanceTypes"]:
            instance_types.append(instance_type["InstanceType"])
