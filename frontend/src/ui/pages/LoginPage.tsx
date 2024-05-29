import {
  Layout,
  Typography,
  Menu,
  Breadcrumb,
  Form,
  InputNumber,
  Input,
  Button,
  Checkbox,
  Space,
} from 'antd'
import { Logo } from '../common/Logo'
import { LockOutlined, UserOutlined } from '@ant-design/icons'

export function LoginPage() {
  return (
    <Layout className="h-screen">
      <Layout.Content className="bg-gray-100 flex flex-col min-h-full justify-center">
        <div className="w-72 flex mx-auto mb-16">
          <Logo full />
        </div>

        <div className="flex">
          <Form
            initialValues={{ remember: true }}
            onFinish={() => {}}
            className="w-96 mx-auto"
          >
            <Form.Item
              name="username"
              rules={[{ required: true, message: 'Please input your Username!' }]}
            >
              <Input
                prefix={<UserOutlined className="site-form-item-icon" />}
                placeholder="Username"
              />
            </Form.Item>
            <Form.Item
              name="password"
              rules={[{ required: true, message: 'Please input your Password!' }]}
            >
              <Input
                prefix={<LockOutlined className="site-form-item-icon" />}
                type="password"
                placeholder="Password"
              />
            </Form.Item>
            <Form.Item>
              <div className="flex justify-between items-center">
                <Form.Item
                  name="remember"
                  valuePropName="checked"
                  noStyle
                >
                  <Checkbox>Remember me</Checkbox>
                </Form.Item>

                <Typography.Text>
                  <a href="/forgot">Forgot password</a>
                </Typography.Text>
              </div>
            </Form.Item>

            <Form.Item>
              <Space
                className="w-full"
                direction="vertical"
                classNames={{ item: 'flex justify-center' }}
              >
                <Button
                  type="primary"
                  htmlType="submit"
                  className="w-full"
                >
                  Log in
                </Button>
                <Typography.Text>
                  or <a href="/register">register now!</a>
                </Typography.Text>
              </Space>
            </Form.Item>
          </Form>
        </div>
      </Layout.Content>

      <Layout.Footer className="py-4">
        <div className="flex items-center justify-center">
          <Typography.Text
            className="text-slate-500"
            code
          >
            Made with ❤️ by{' '}
            <a
              className="!text-blue-400 !font-bold"
              href="https://github.com/QuanticTeam/AtomSkills2024"
            >
              QuanticTeam
            </a>
          </Typography.Text>
        </div>
      </Layout.Footer>
    </Layout>
  )
}
