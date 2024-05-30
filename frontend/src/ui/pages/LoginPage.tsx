import { z } from 'zod'
import { LockOutlined, UserOutlined } from '@ant-design/icons'
import { Alert, Button, Checkbox, Form, Input, Space, Typography } from 'antd'
import { isStrongPassword } from 'validator'
import { Logo } from '../common/Logo'
import { PublicTemplate } from '../templates/PublicTemplate'
import { Link } from 'react-router-dom'

function getIsPasswordStrong(value: string, returnScore = false): boolean | number {
  return isStrongPassword(value, {
    minLength: 8,
    minLowercase: 1,
    minUppercase: 1,
    minNumbers: 1,
    minSymbols: 1,
    returnScore: returnScore as any,
    pointsPerUnique: 1,
    pointsPerRepeat: 0.5,
    pointsForContainingLower: 10,
    pointsForContainingUpper: 10,
    pointsForContainingNumber: 10,
    pointsForContainingSymbol: 10,
  })
}

const LoginData = z.object({
  password: z
    .string()
    .trim()
    .min(1)
    .min(8)
    .max(32)
    .regex(/^[A-Za-z0-9-_+!?=#$%&@^`~]+$/, 'Unacceptable symbols')
    .refine(p => !getIsPasswordStrong(p), 'Password is too weak'),
  remember: z.boolean(),
  username: z.string().min(2).max(56),
})

// console.log(LoginData.parse({ username: 'Ludwig', password: '~а~~~~~~~', remember: false }))

type LoginData = z.infer<typeof LoginData>

export function LoginPage() {
  return (
    <PublicTemplate>
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
              prefix={<UserOutlined className="text-gray-400" />}
              placeholder="Пользователь"
            />
          </Form.Item>
          <Form.Item
            name="password"
            rules={[{ required: true, message: 'Please input your Password!' }]}
          >
            <Input
              prefix={<LockOutlined className="text-gray-400" />}
              type="password"
              placeholder="Пароль"
            />
          </Form.Item>
          <Form.Item>
            <div className="flex justify-between items-center">
              <Form.Item
                name="remember"
                valuePropName="checked"
                noStyle
              >
                <Checkbox>Запомнить меня</Checkbox>
              </Form.Item>

              <Typography.Text>
                <Link to="/forgot">Не помню пароль</Link>
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
                Войти
              </Button>
              <Typography.Text>
                или <Link to="/register">зарегистрироваться</Link>
              </Typography.Text>
            </Space>
          </Form.Item>
        </Form>
      </div>
    </PublicTemplate>
  )
}
